package fos

import scala.collection.mutable.{ Map, HashMap };
import scala.language.postfixOps

case class TypeError(msg: String) extends Exception(msg)

object Type {

    import CT._
    import Utils._

    type Class = String
    type Context = scala.collection.immutable.Map[String, Class]

    //added by Valerian
    val OK: Class = "OK"
    /* the result ok is required when typechecking classes. This is the
   * produced result when no problem occured in class typechecking.
   * (see notation used in reference paper) */

    def typeOf(tree: Tree, ctx: Context): Class = tree match {
        //   ... To complete ...
        /* code added by Valerian */
        // this case should not occur (see eval method in FJ.scala
        case Program(cls, expr) => {
            cls foreach { typeOf(_, ctx) }
            typeOf(expr, ctx)
        }
        // the cases are treated in order given by reference paper

        // T-VAR
        case Var(name) => ctx.get(name) match {
            case None => throw TypeError("variable " + name + " is not defined.")
            case Some(typ) => typ
        }
        // T-FIELD
        case Select(obj, field) => {
            val classDef = typeOfExpr(obj, ctx)
            classDef findField field match {
                case None =>
                    throw TypeError(classDef.name + " does not contain field " + field)
                case Some(FieldDef(tpe, name)) => tpe
            }
        }
        // T-INVK
        case Apply(obj, method, args) => {
            val classDef = typeOfExpr(obj, ctx)
            val methodDef = classDef findMethod method getOrElse {
                throw TypeError("method " + method + " is not defined in " + classDef.name)
            }
            methodDef checkTypeArguments (args map (typeOf(_, ctx)))
            methodDef.tpe
        }
        case _ => ???
        /* end of code added by Valerian */
    }

    //added by valerian
    def typeOfExpr(expr: Expr, ctx: Context): ClassDef = {
        val className = typeOf(expr, ctx)
        lookup(className) match {
            case None => throw TypeError("type " + className + " is not defined")
            case Some(classDef) => classDef
        }
    }

}

case class EvaluationException(msg: String) extends Exception

object Evaluate extends (Expr => Expr) {

    import Utils._

    def apply(expr: Expr): Expr = ???
    //   ... To complete ... 

    def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef, Expr)]): Expr = exp match {
        case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
        case New(cls, args) => New(cls, args map (arg => substituteInBody(arg, thiss, substs)))
        case Cast(cls, e) => Cast(cls, substituteInBody(e, thiss, substs))
        case Var("this") => thiss
        case Var(bd) => substs find (subs => subs._1.name == bd) match {
            case None => exp
            case Some((_, sub)) => sub
        }

        case Apply(obj, method, args) => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
        case _ => throw new EvaluationException("Apply: Forgot expression " + exp)
    }
}

object CT {

    val objectClass: String = "Object"
    private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil), Nil)

    private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]

    add(objectClass, objectClassDef)

    def elements = ct iterator

    def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

    def add(key: String, element: ClassDef): Unit = ct += key -> element

    def delete(key: String) = ct -= key

    def clear(): Unit = {
        ct clear;
        add(objectClass, objectClassDef)
    }

    /*-------------------------------------------------------*/
    // Code added by Valerian

    def firstInheritanceLoop: Option[ClassDef] = ct.values find { classDef =>
        classDef isSuperclassOf classDef.superClass
    }

    def checkInheritanceLoop: Unit = firstInheritanceLoop match {
        case None => ()
        case Some(classDef) =>
            throw new ClassHierarchyException(classDef.name + "is part of an inheritance loop")
    }

    // end of code added by Valerian
    /*--------------------------------------------------------*/

}

object Utils {

    def getClassDef(className: String): ClassDef = CT lookup className match {
        case None => throw new TypeError("class " + className + " not declared")
        case Some(c: ClassDef) => c
    }
}
