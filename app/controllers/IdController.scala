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
    val deterMineResults = for {i <- 0 to candles.length - 3} yield doit(candles(i).close, candles(i + 1).close, candles(i + 2).close)
    println(deterMineResults)
    val betterDeterMineResults = deterMineResults.flatMap(_.toList)
    val calculateResults = for {i <- 0 until betterDeterMineResults.length} yield calculate(
      betterDeterMineResults(i)._1, betterDeterMineResults(i)._2).mkString("", "\n", "")
    Ok(views.html.dashboard(calculateResults.toString))
    //Ok(views.html.dashboard(deterMineResults))
    //Ok(views.html.dashboard(candles.toString))
  }
   //37行目のループ、def calcuの計算式を組み立てる

  // false = sell
  // true = buy
  def doit(A1: Double, A2: Double, A3: Double): Option[(Boolean, Double)] = {
    if (A2 - A1 <= 0 && A3 - A2 > 0) {
      Some(true, A3)
    } else if (A2 - A1 <= 0 && A3 - A2 <= 0) {
      Some(false, A3)
    } else None
  }

  def calculate(Aa: Boolean, Ab: Double): Option[(Boolean, Double)] = {
    if (true) {
      Some(true, Ab)
    } else if (false) {
      Some(false, Ab)
    }else None
  }


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