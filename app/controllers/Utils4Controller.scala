package controllers

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser._

import scala.sys.process._

object Utils4Controller extends LazyLogging {

  def getCandles: List[Zaif.B] = {
    val f: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    val localDate: LocalDate = LocalDate.from(f.parse("2018-01-20 16:00"))
    val zoneId: ZoneId = ZoneId.systemDefault
    require(zoneId.toString == "Asia/Tokyo")
    val from: Long = localDate.atStartOfDay(zoneId).toEpochSecond
    val to: Long = LocalDate.now(zoneId).atStartOfDay(zoneId).toEpochSecond
    val x: String = (s"curl https://zaif.jp/zaif_chart_api/v1/history?symbol=XEM_JPY&resolution=30&from=$from&to=$to" !!).replace("""\""", "").trim.drop(1).dropRight(1)
    parse(x).fold(throw _, identity).as[Zaif.A].fold(throw _, identity).ohlc_data
  }

  Files.write(Paths.get("output.csv"), Zaif.A(getCandles).toCsv.getBytes(StandardCharsets.UTF_8))
}

object Zaif {

  // case class B(volume: Double, average: Double, high: Double, low: Double, time: Long, close: Double, open: Double)
  case class B(time: Long, open: Double, high: Double, low: Double, close: Double) {
    def toCsvRow: String = (time :: open :: high :: low :: close :: Nil).map(_.toString).reduce(_ + "," + _)
  }

  case class A(ohlc_data: List[B]) {
    def toCsv: String = "date,open,high,low,close\n" + ohlc_data.sortBy(_.time).map(_.toCsvRow).reduce(_ + "\n" + _)
  }
}