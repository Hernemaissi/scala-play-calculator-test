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

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action {
    Ok("Hello World!")
  }
  
  def calculus(query:String) = Action {implicit request =>
    var parsedQuery : String = new String(Base64.getDecoder().decode(query))
    var postfix = Calculus.toPostfix(parsedQuery)
    var result = Calculus.evaluate(postfix)
    var jsonResult : JsObject = Json.obj("error" -> true)
    if (Calculus.isError(result)) {
        result = Calculus.getErrorMsg(result)
        jsonResult = Json.obj("error" -> true, "message" -> result)
        BadRequest(jsonResult)
    } else {
       jsonResult = Json.obj("error" -> false, "result" -> result.toInt) 
       Ok(jsonResult)
    }
  }

}
