package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import java.util.Base64
import java.nio.charset.StandardCharsets
import play.api.libs.json._
import models.Calculus

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  
  def index = Action {
    Ok("Usage: /calculus?query=[base64encodedstring]")
  }
  
  /**
   * This action receives a string representing an equation in base64 encrypted form
   * It decrypts it, converts it to postfix and then evaluates the result
   * Returns either result in JSON or 400 bad request and and a reason message in JSON
   */
  def calculus(query:String) = Action {implicit request =>
    var parsedQuery : String = new String(Base64.getDecoder().decode(query))
    var postfix = Calculus.toPostfix(parsedQuery)
    var result = Calculus.evaluate(postfix)
    if (Calculus.isError(result)) {
        result = Calculus.getErrorMsg(result)
        var jsonResult = Json.obj("error" -> true, "message" -> result)
        BadRequest(jsonResult)
    } else {
        var jsonResult = Json.obj("error" -> false, "result" -> result.toInt) 
        Ok(jsonResult)
    }
  }

}
