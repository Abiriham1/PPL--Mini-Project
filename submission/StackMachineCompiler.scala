package edu.colorado.csci3155.project1

object StackMachineCompiler {
    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        /* Begin Solution */
       e match {
            case Const(f) => List(PushNumI(f))
            case BoolConst(b) => List(PushBoolI(b)) 
            case Ident(id) => List(StoreEnv(id))

            // arthmetic 
            case Plus(e1, e2) => {compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(AddI) } // Plus takes in two parameters e1 and e2. returns concate list 
            case Minus(e1, e2) => {compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(SubI) }
            case Mult(e1,e2) => {compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(MultI) }
            case Div(e1,e2) => {compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(DivI)}

            // Exp and Trig
            case Exp(e1) => {compileToStackMachineCode(e1) ++ List(ExpI) }
            case Log(e1) => {compileToStackMachineCode(e1) ++ List(LogI) }
            case Sine(e1) => {compileToStackMachineCode(e1) ++ List(SinI)}
            case Cosine(e1) => {compileToStackMachineCode(e1) ++ List(CosI) }

            // Logigical operations
            case Geq(e1,e2) => {compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(GeqI)}
            case Eq(e1,e2) => {compileToStackMachineCode(e1) ++ List(EqI) }
            case Not(e1) => {compileToStackMachineCode(e1) ++ List(NotI)}
            case Let(id,e1,e2) => {
                compileToStackMachineCode(e1) ++ List(LoadEnv(id)) ++ compileToStackMachineCode(e2) ++ List(PopEnv) // return the concate list
            }
             case IfThenElse(cond, e1, e2) => {
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                /* return list of instruction   */
                compileToStackMachineCode(cond) ++ List(CSkipI(l1.length + 1)) ++ l1 ++ List(SkipI(l2.length)) ++ l2
                }

           case And(e1, e2) => {
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                l1 ++ List(CSkipI(l1.length + 1)) ++ l2 ++ List(SkipI(1)) ++ List(PushBoolI(false))
                }
            case Or(e1, e2) => {
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                l1 ++ List(CSkipI(l1.length - 1)) ++ List(PushBoolI(true)) ++ List(SkipI(l2.length)) ++ l2
                }
            case _ => throw new IllegalArgumentException(s"I do not handle $e")
         }
         /* End Solution */
     }
 }
