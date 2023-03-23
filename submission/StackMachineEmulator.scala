package edu.colorado.csci3155.project1

import scala.annotation.tailrec

sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {

        
    







            case AddI => stack match {
 
                case Num(value1) :: Num(value2) :: rest => (Num(value1 + value2) :: rest, env) // check the two num() values in OpStack, and if so pop the values. 
                case _ :: _ :: _ => throw new Exception("Invalid argument ") // if both values are not Num() then excutete this
                case _ => throw new Exception("You have an empty stack")    // if the stack is empty stack 

            }
            case SubI => stack match {

                case Num(value1) :: Num(value2) :: rest => (Num(value2 - value1) :: rest, env) // check the two num() values in OpStack, and if so pop the values. Then do subtractions
                case _ :: _ :: _ => throw new Exception("Invalid argument  ") // if both values are not Num() then excutete this
                case _ => throw new Exception(" You have an empty stack") // if the stack is empty stack 
            }
            case MultI => stack match {
                case Num(value1) :: Num(value2) :: rest => (Num(value1 * value2) :: rest, env)//checks that top two Num values in OpStack if so pop the values off stack  multiply the two values
                case _ :: _ :: _ => throw new Exception("Invalid argument ")// if both values are not Num() then excutete this
                case _ => throw new Exception("You have an empty stack")// if the stack is empty stack 
            }
            case DivI => stack match {
                case Num(value) :: Num(value2) :: rest if value != 0 => (Num(value2 / value) :: rest, env)//checks that top two Num values in OpStack if so pop the values off stack  divide the two values
                case Num(value) :: Num(value2) :: _ => throw new Exception("Trying to divid by 0")// If the first Num(_) value is zero, throw an excep.
                case _ :: _ :: _ => throw new Exception("Invalid argument ") // if the values are not Num()
                case _ => throw new Exception("Empty stack")// if the stack is empty stack 
            }
            case ExpI => stack match {
                case Num(value) :: rest => (Num(math.exp(value)) :: rest, env) //given an Num(_) value at the top of stack, pop off stack, and push onto stack its exponential value
                case _ => throw new Exception("Invalid argument / Stack is Empty") // If the stack is empty 
            }
            case LogI => stack match {
                case Num(value) :: rest if value > 0 => (Num(math.log(value)) :: rest, env) // if Num(_) value is at the top of stack, then pop it off the stack, and push its logarthmic value into the stack 
                case Num(value) :: _ => throw new Exception("Cannot take the log of a non-positive number") // if Num() value is zero throw a error
                case _ :: _ => throw new Exception("Invalid argument ")     // if there is  invalid numerical value throw an excep.
                case _ => throw new Exception("Cannot take the log of an empty stack")//  case for if stack is empty
            }
            case SinI => stack match {
                case Num(value) :: rest => (Num(math.sin(value)) :: rest, env) // if numerical(_) value is at the top of stack then you should pop it off the stack, and push its sine value into the stack
                case _ => throw new Exception("Invalid argument ") // case for invalide argument types 
            }

            case CosI => stack match {
                case Num(value) :: rest => (Num(math.cos(value)) :: rest, env) // if Num(_) value is at the top of stack then you should pop it off the stack, and push its sine value into the stack
                case _ => throw new Exception("Invalid argument ") // if the stack is empty throw this error message
            }
            case GeqI => stack match {
                case Num(value) :: Num(value2) :: rest => (Bool(value2 >= value) :: rest, env) // if the 2 elements in the stack are numerical(), then pop them off and pish the boolean statement into the stack. 
                case _ :: _ :: _ => throw new Exception("Invalid argument types ") // throw this invalid arguemnt when the top 2 elemets are not numerical 
                case _ => throw new Exception("Empty stack") // this this error message if the stack is empty. 
            }
            case EqI => stack match {
                case value1 :: value2 :: rest if value1 == value2 => (Bool(true) :: rest, env) // if the top 2 elements in the stack are equal then pop them off and push bool true into the stack.
                case value1 :: value2 :: rest if value1 != value2 => (Bool(false) :: rest, env) // if the two values are not the same then push bool false to the stack
                case _ => throw new Exception("Empty stack") // if the stack is empty
            }

            case NotI => stack match {
                case Bool(b) :: rest => (Bool(!b) :: rest, env) // if boolean is at the top of the stack, pop it off and negate the boolen and push it onto the stack. 
                case _ => throw new Exception("Not Boolean or Empty Stack") // if the stack is empty evaluate this line
            }
           case LoadEnv(s) => stack match {
                case Nil => throw new Exception("empty stack")
                case v :: rest => 
                    val idVal = (s, v) // collects (identifier, value)
                    (rest, idVal :: env) //rest is now the remaining elements of runtime stack
            }      
            case StoreEnv(s) =>
                env.find { case (id, _) => id == s } match {
                    case Some((_, vale)) => (vale :: stack, env) 
                    case None => throw new Exception(s"No value found for identifier on op $s") 
            }
            case PopEnv => (stack, env.tail)
            case PushNumI(d) => (Num(d) :: stack, env)  //push a Num(d) and bool(b) onto OpStack
            case PushBoolI(b) => (Bool(b) :: stack, env)  //push a Num(d) and bool(b) onto OpStack
            case PopI =>
                if(stack.isEmpty)
                {
                throw new RuntimeException("PopI on Empty Operand Stack ") // throw an exeption if the runtime stack is empty
                } else 
                {
                    (stack.tail, env)
                }
            

            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }





    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}