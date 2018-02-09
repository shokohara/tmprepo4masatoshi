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

import scala.collection.immutable
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
    val averageLine26 = {averageCalA(Utils4Controller.getCandles.map(_.close), 26)}
    val averageLine6 = {averageCalB(Utils4Controller.getCandles.map(_.close), 6)}
    //List - Listの計算
    val averageDiff: List[Double] = (0 until averageLine26.length).map(i => averageLine26(i) - averageLine6(i)).toList
    val myAssetsResult = myAssetsCal(Utils4Controller.getCandles.map(_.close),
      Utils4Controller.getCandles.map(_.time),averageDiff)
    //ここから最小二乗法
    //26分足=y, 6分足=x
    val deviation26 = {deviationCal(Utils4Controller.getCandles.map(_.close),averageLine26)}
    val deviation6 = {deviationCal2(Utils4Controller.getCandles.map(_.close),averageLine6)}
    //分散のため、6分足(x)二乗後に平均化
    val deviationSquaring =(0 until averageLine6.length).map(i=> deviation6(i) * deviation6(i)).toList
    val dispersion = {despersionCal(deviationSquaring, 6)}
    //共分散のため、26分足と6分足を乗算後、平均化
    val covarianceSquaring = (0 until averageLine26.length).map(i=> deviation26(i) * deviation6(i)).toList
    val covariance: List[Double] = {covarianceCal(covarianceSquaring, 26)}
    val Slope = (0 until covariance.length).map(i => covariance(i) / dispersion(i)).toList
    println(covariance)
    Ok(views.html.virtualCurrency(myAssetsResult.toString))
  }

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

  def averageCalA(values: List[Double], period: Int): List[Double] = {
    (for (i <- 1 to values.length)
      yield
        if (i < period) 0.00
        else {values.slice(i - period, i).reduceLeft(_ + _) / period
        }).toList
  }

  def averageCalB(values: List[Double], period: Int): List[Double] = averageCalA(values, period)
    //averageCalA(List.empty[Double], 26)
    //averageCalA(List.empty[Double], 6)

  // def myAssetsCal(xemClosePriceDateAvg: List[(Double,Long,Double)]):Double = {
  def myAssetsCal(xemClosePrices: List[Double], xemClosePriceDates:List[Long], averageDiff :List[Double]):Double= {
    // xemClosePrices,dates,averageDiffの先頭が多分古くあるべき
    // averageDiffとclosePricesの長さがもし違うのであれば（実装社依存）下記の通りにzipするべき
    // [1,2,3,4,5] [x,y,z] => [3,4,5] [x,y,z]
    // my bad i+1は落ちる気がするIndexArrayOutBoundsOfExceptionで
    require(xemClosePrices.length == xemClosePriceDates.length)
    require(averageDiff.length <= xemClosePrices.length)
    var myAsset0:Double = 1000000
    var myAsset1:Double = 100
    // 直近のxemの終値 -> 古い順でソートしたxemの終値の一番最初の値
    // 日本円換算
    val firstFund = myAsset0 + xemClosePrices.head * myAsset1
    val ratio = 0.99
    var count = 0
    var buy: List[(Double,Long)] = List.empty[(Double,Long)]
    var sell: List[(Double,Long)] = List.empty[(Double,Long)]
    for (i <- 0 until xemClosePrices.length - 1){
      if(averageDiff(i) < 0 && averageDiff(i+1) > 0 && myAsset0 != 0){
        myAsset1 = myAsset1 + ((myAsset0/xemClosePrices(i+1))*ratio)
        myAsset0 = 0
        count = count + 1
        buy = buy ++ List((xemClosePrices(i+1), xemClosePriceDates(i+1)))
      }else if (averageDiff(i) > 0 && averageDiff(i+1) < 0 && myAsset1 != 0) {
        myAsset0 = myAsset0 + ((myAsset1*xemClosePrices(i+1)) * ratio)
        myAsset1 = 0
        count = count + 1
        sell = sell ++ List((xemClosePrices(i+1), xemClosePriceDates(i+1)))
      }else None
    }
    val endFund = myAsset0 + xemClosePrices.last * myAsset1
    // 割合がほしいのでよしなに割り算して返す
    endFund
  }

  def deviationCal(xemClosePrices: List[Double], xemAverageLine26: List[Double]): List[Double] = {
    //require(xemClosePrices(0 to 5) == xemAverageLine26.length)
    xemClosePrices.zipAll(xemAverageLine26, 0.0, 0.0)
      .map { case (a, b) => if (a == 0.0 || b == 0.0) None else Some((a, b)) }
      //map{case}はList内の計算
      .map(_.map { case (a, b) => a - b })
      .map(_.getOrElse(0.0))
  }

  def deviationCal2(xemClosePrices: List[Double], xemAverageLine6: List[Double]): List[Double] =
    deviationCal(xemClosePrices,xemAverageLine6)


  def despersionCal(xemDeviationSquaring: List[Double],period: Int):List[Double] = {
    (for (i <- 1 to xemDeviationSquaring.length)
      yield
        if (i < period) 0.00
        else {xemDeviationSquaring.slice(i - period, i).reduceLeft(_+_) / period
        }).toList
  }

  def covarianceCal(xemCovarianceSquaring: List[Double],period: Int):List[Double] = {
    (for (i <- 1 to xemCovarianceSquaring.length)
      yield
        if (i < period) 0.00
        else {
          xemCovarianceSquaring.slice(i - period, i).reduceLeft(_ + _) / period
        }).toList
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