package common

import scala.util.{Failure, Success, Try}

object TryAll {
  def apply[T](f: =>T): Try[T] = {
    try{
      Success(f)
    }catch{
      case e: Throwable => Failure(e)
    }
  }
}
