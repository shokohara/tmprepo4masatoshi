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
    val betterDeterMineResults = deterMineResults.flatMap(_.toList)
    val calculateResults = for {i <- 0 until betterDeterMineResults.length} yield function(
      betterDeterMineResults(i)._1, betterDeterMineResults(i)._2)
    val temp = calculateResults.sum

    //varの場合
    //var temp:Double = 0
    //for (i <- (0 until calculateResults.length)) temp = calculateResults(i) + temp

    println(temp)
    Ok(views.html.dashboard(calculateResults.toString))
    //.mkString("", "\n", "")
    //Ok(views.html.dashboard(deterMineResults))
    //Ok(views.html.dashboard(candles.toString))
  }

  val virtualCurrency = Action {implicit request =>
    val candles: List[Zaif.B] = Utils4Controller.getCandles
    val averageLineA = averageCal(candles(0).close,candles(0).time)
    Ok(views.html.virtualCurrency(averageLineA))
  }


   //
   //taple option,double option double

  // false = sell
  // true = buy
  def doit(A1: Double, A2: Double, A3: Double): Option[(Boolean, Double)] = {
    if (A2 - A1 <= 0 && A3 - A2 > 0) {
      Some(true, A3)
    } else if (A2 - A1 <= 0 && A3 - A2 <= 0) {
      Some(false, A3)
    } else None
  }

  def function(List:(Boolean, Double)): Double= {
    if (List._1 == false) {
        List._2
    } else {
      List._2 * -1.001
    }
  }

  def averageCal(values: List[Double], period: Long): List[Double] = {
    (for (i <- 1 to values.length)
      yield
        if (i < period) 0.00
        else {values.slice(i - period, i).reduceLeft(_ + _) / period
        }).toList
  }

  /*def averageCal(A1: Double, A2: Double, A3: Double):Option[(Boolean, Double)] = {
    if
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