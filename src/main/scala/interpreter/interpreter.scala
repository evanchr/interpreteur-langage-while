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

  // TODO commenter

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
      case Nil => NlValue
      case (Var(name1),valu) :: tl => v match { case Var(name2) => if (name1 == name2) valu else lookUp(v, tl) }
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
      case Nil => List((v,d))
      case (Var(name1),valu) :: tl =>
        v match {
          case Var(name2) => if (name1 == name2) { (v,d) :: tl }
                             else { (Var(name1),valu) :: assign(v,d,tl) }
        } 
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
    expression match {
      case Nl => NlValue
      case Cst(name) => CstValue(name)
      case VarExp(name) => val v: Variable = Var(name) ; lookUp(v,mem)
      case Cons(arg1,arg2) => ConsValue(interpreterExpr(arg1,mem), interpreterExpr(arg2, mem))
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
        else ConsValue(NlValue,NlValue)
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
    value match {
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1),valueToExpression(arg2))
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
    command match {
      case Nop => memory
      case Set(v,e) => assign(v,interpreterExpr(e, memory), memory)
      case While(cond,body) => 
        interpreterExpr(cond, memory) match {
          case NlValue => memory
          case _ => interpreterCommand(While(cond,body), interpreterCommands(body, memory))
        }
      case For(count,body) => 
        interpreterExpr(count, memory) match {
          case NlValue => memory
          case valu => interpreterCommand(For(Tl(valueToExpression(valu)),body), interpreterCommands(body, memory))
        }
      case If(cond,then_cmd,else_cmd) => {
        interpreterExpr(cond, memory) match {
          case NlValue => interpreterCommands(else_cmd, memory)
          case _ => interpreterCommands(then_cmd, memory)
        }
      }
    }
  }
  
  
  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil => throw ExceptionListeVide
      case hd :: Nil => interpreterCommand(hd, memory)
      case hd :: tl => interpreterCommands(tl, interpreterCommand(hd, memory))
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
      case (Nil,Nil) => throw ExceptionListeVide
      case (_,Nil) | (Nil,_) => throw ExceptionListesDeLongueursDifferentes
      case (hd::Nil,hd2::Nil) => (hd,hd2) :: Nil
      case (hd::tl,hd2::tl2) => (hd,hd2) :: interpreterMemorySet(tl,tl2)  
    }
  }
  

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = ???

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreter(program: Program, vals: List[Value]): List[Value] = ???
  
  def main(args: Array[String]): Unit = {
    val memory1: Memory = List(
      (Var("X"), ConsValue(NlValue, ConsValue(NlValue, NlValue))))
    val expected: Memory = List(
      (
        Var("X"),
        ConsValue(
          NlValue,
          ConsValue(
            NlValue,
            ConsValue(
              NlValue,
              ConsValue(
                NlValue,
                ConsValue(
                  NlValue,
                  ConsValue(
                    NlValue,
                    ConsValue(
                      NlValue,
                      ConsValue(NlValue, NlValue))))))))))
    val result =
      interpreterCommand(
        For(
          VarExp("X"),
          List(
            For(
              VarExp("X"),
              List(
                Set(Var("X"), Cons(Nl, VarExp("X"))))))),
        memory1)
    println(result)
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