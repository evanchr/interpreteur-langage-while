package interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 * 
 * ETUDIANT 1 : KAWRANTIN ARZEL--GUIZIOU
 * 
 * ETUDIANT 2 : CHARRIER EVAN
 * 
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil => NlValue // on renvoie la val nule si la variale n'existe pas dans la mémoire
      case (vari,valu) :: tl => if (v == vari) valu else lookUp(v, tl)
        // on compare les noms des vars et on renvoie sa valeur si il y a correspondance
        // ou on continue de parcourir la mémoire sinon
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case Nil => List((v,d)) // si la memoire est vide, on rajoute un nouveau couple (var,val)
      case (vari, valu) :: tl =>
        if (v == vari) { (v,d) :: tl }
          // s'il y a correspondance entre v et une variable dans la memoire, on renvoit un couple (v,d) suivi du reste de la memoire
        else { (vari,valu) :: assign(v,d,tl) }
          // sinon, on reprend le couple actuel et on continu la construction de la memoire en rappelant assign sur la suite de la memoire
    } 
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    // ici, on prendra chaque class du trait Expression et on renverra la valeur qui lui correspond, parfois en operant
    expression match {
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => lookUp(Var(name),mem) // ici, il s'agit tout simplement de reutiliser la fonction lookUp
      case Cons(arg1,arg2) => ConsValue(interpreterExpr(arg1,mem), interpreterExpr(arg2, mem)) // appel recursif sur les args en parametre
      case Hd(arg) => 
        interpreterExpr(arg, mem) match {
          case ConsValue(head, _) => head
          case _ => NlValue
        }
      case Tl(arg) => 
        interpreterExpr(arg, mem) match {
          case ConsValue(_, tail) => tail
          case _ => NlValue
        }
      case Eq(arg1,arg2) =>
        if (interpreterExpr(arg1, mem) != interpreterExpr(arg2, mem)) NlValue
          // s'il n'y a pas correspondance, on renvoie la valeur nulle
        else ConsValue(NlValue,NlValue)
          // s'il y a, on renvoit une "autre valeur", le cours nous disait que la valeur "Cons(nil,nil)" convenait par exemple
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    // on effectue l'operation inverse de la fonction precedente :
    // pour chaque class du trait Value, on renvoit son expression correspondante
    value match {
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1),valueToExpression(arg2)) // appel recursif sur les args en parametre
    }
  }
  

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    // on execute une commande sur la memoire, et on renvoit la memoire changee
    // certaines commandes ont une liste de commandes en parametre, on utilisera interpreterCommands pour les executer
    command match {
      case Nop => memory // pas de commande, donc pas de modification de memoire
      case Set(v,e) => assign(v,interpreterExpr(e, memory), memory) // on reutilise assign pour modifier la valeur en memoire
      case While(cond,body) =>
        interpreterExpr(cond, memory) match {
          case NlValue => memory
            // si la cond a pour valeur nil, il n'y a pas/plus de modifications de memoire a faire
          case _ => interpreterCommand(While(cond,body), interpreterCommands(body, memory))
            // on effectue les memes commandes jusqu'a ce la cond soit egale a nil
        }
      case For(count,body) =>
        interpreterExpr(count, memory) match {
          case NlValue => memory
          case valu => interpreterCommand(For(Tl(valueToExpression(valu)),body), interpreterCommands(body, memory))
            // on decremente la cond en prenant l'expression de droite de la valeur convertie en expression
            // a part cela, le fonctionnement est similaire a While
        }
      case If(cond,then_cmd,else_cmd) =>
        interpreterExpr(cond, memory) match {
          case NlValue => interpreterCommands(else_cmd, memory)
            // si le test d'egalite echoue, on execute les commandes "else"
          case _ => interpreterCommands(then_cmd, memory)
            // sinon on execute les commandes "then"
        }
    }
  }
  
  
  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    // on execute une commande sur la memoire, et on renvoit la memoire changée
    // cette méthode sert à éxécuter les listes de commandes
    commands match {
      case Nil => throw ExceptionListeVide // la liste des commandes est vide
      case hd :: Nil => interpreterCommand(hd, memory) // il reste une seule commande dans la liste de commande donc on l'éxécute avec interpreterCommand
      case hd :: tl => interpreterCommands(tl, interpreterCommand(hd, memory))
        // dans ce dernier cas, on interprète la première commande de la liste (hd) qui change une première fois la mémoire
        // puis on appelle récursivement la méthode afin de traiter la liste des commandes restantes (tl) 
        // qui prend en paramétre la mémoire déjà changé par la première commande etc.. jusqu'a ce que la liste de commandes soit vide
    }
  }
  

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    (vars,vals) match {
      case (Nil,Nil) => throw ExceptionListeVide // cas où les 2 listes sont vides
      case (_,Nil) | (Nil,_) => throw ExceptionListesDeLongueursDifferentes // cas où une des 2 listes se termine avant l'autre
      case (hd::Nil,hd2::Nil) => (hd,hd2) :: Nil // cas où il y a uniquement 1 variable à associer à 1 valeur
      case (hd::tl,hd2::tl2) => (hd,hd2) :: interpreterMemorySet(tl,tl2)  
        // dans ce dernier cas, on crée une mémoire avec comme premier élément (variable,valeur) associé
        // puis on fait un appel récursif sur le reste des variables et valeurs à associer
        // on concatène cet appel au premier élément
    }
  }
  

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    vars match {
      case Nil => throw ExceptionListeVide // cas où la liste des variables à chercher est vide
      case head :: Nil => lookUp(head, memory) :: Nil // cas où l'on cherche une seule variable
      case head :: next => lookUp(head, memory) :: interpreterMemoryGet(next, memory)
        // dans ce dernier cas, on cherche avec la méthode lookUp la première variable dans la mémoire
        // puis on fait un appel récursif sur le reste des variables à rechercher
        // on concatène cet appel au premier élément afin de créer une liste de valeurs à retourner
    }
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
    // on cherche ici à recréer le programme en fonction de toutes les méthodes
    program match {
      case Progr(in, body, out) => 
        var mem = interpreterMemorySet(in,vals) // on affecte les variables d'entrée à leurs valeurs
        mem = interpreterCommands(body, mem) // on change la mémoire en fonction de la liste de commandes à effectuer
        interpreterMemoryGet(out,mem) // on récupère les variables avec leurs nouvelles valeurs de sortie
    }
  }
  

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}