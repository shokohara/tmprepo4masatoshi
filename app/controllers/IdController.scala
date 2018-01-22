package controllers

import javax.inject._

import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.Forms.text
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.i18n._
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class IdController @Inject()(repo: PersonRepository,
                             cc: MessagesControllerComponents
                            )(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) {
  val bodyForm: Form[UserData] = Form {
    mapping(
      "name" -> text,
      "body" -> text
    )(UserData.apply)(UserData.unapply)
  }

  val index = Action { implicit request =>
    Ok(views.html.bodyIndex(bodyForm))
  }

  val dashboard = Action { implicit request =>
    val candles: List[Zaif.B] = Utils4Controller.getCandles
    val result: String = doit(candles(0).close, candles(1).close).toString()
    Ok(views.html.dashboard(result))
        //Ok(views.html.dashboard(candles.toString))
  }

  // false = sell
  // true = buy
  def doit(previous: Double, current: Double): (Boolean, Double) =(true,current)
    /*{if(previous < (previous + 1)){
     // 現在の価格が５分前の価格より１円高かった場合、buy実行
    }else{
      //現在の価格が５分前の価格より１円低かった場合、sell実行
    }
    }*/

  def input = Action { implicit request =>
    Ok(views.html.bodyIndex(bodyForm))
  }

  def result = Action { implicit request =>
    bodyForm.bindFromRequest().fold(
      errorForm => {
        //error
        Ok(views.html.bodyIndex(errorForm))
      },
      requestForm => {
        println(bodyForm)
        Ok(views.html.bodyIndex(bodyForm.fill(requestForm)))
      }
    )
  }
}

case class UserData(name: String, body: String)