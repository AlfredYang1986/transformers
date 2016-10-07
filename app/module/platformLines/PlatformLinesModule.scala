package module.platformLines

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms.smsModule

import java.util.Date
import java.util.Calendar

import module.auth.registerTypes
import module.auth.AuthModule

object lineStatus {
  case object pushed extends lineStatusDefines(0, "pushed")
  case object fulled extends lineStatusDefines(1, "full")
}

sealed abstract class lineStatusDefines(val t : Int, val des : String)

object PlatformLinesModule {
    def platformLinePush(data : JsValue) : JsValue = {
        try {
            val pl_id = Sercurity.md5Hash("platform lines: " + Sercurity.getTimeSpanWithMillSeconds)
            val builder = MongoDBObject.newBuilder
            (data \ "line").asOpt[JsValue].map { x =>
                val lb = MongoDBObject.newBuilder
                lb += "origin_province" -> (x \ "origin_province").asOpt[String].get
                lb += "origin_city" -> (x \ "origin_city").asOpt[String].get
                lb += "destination_province" -> (x \ "destination_province").asOpt[String].get
                lb += "destination_city" -> (x \ "destination_city").asOpt[String].get
                
                builder += "line" -> lb.result
            }.getOrElse(throw new Exception("wrong input"))
            
            (data \ "vehicle").asOpt[List[String]].map { x =>
                builder += "vehicle" -> x  
            }.getOrElse(throw new Exception("wrong input"))
            
            (data \ "vehicle_length").asOpt[List[Float]].map { x =>
                builder += "vehicle_length" -> x
            }.getOrElse(throw new Exception("wrong input"))
            
            builder += "weight" -> (data \ "weight").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "volume" -> (data \ "volume").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "notes" -> (data \ "notes").asOpt[String].map (x => x).getOrElse("")
          
            (data \ "date_str").asOpt[String].map { x =>
                val sdf = new java.text.SimpleDateFormat("MM/dd/yyyy")
                builder += "date" -> sdf.parse(x).getTime
            }.getOrElse(throw new Exception("wrong input"))
          
            builder += "push_date" -> new Date().getTime
            builder += "status" -> lineStatus.pushed.t
            builder += "pl_id" -> pl_id
            
            _data_connection.getCollection("platformlines") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(builder.result))))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def platformLinePop(data : JsValue) : JsValue = {
        try {
            val pl_id = (data \ "pl_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "platformlines" where ("pl_id" -> pl_id) select (x => x)).toList match {
              case head :: Nil => {
                _data_connection.getCollection("platformlines") -= head
                toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def platformLineUpdate(data : JsValue) : JsValue = {
        try {
            val pl_id = (data \ "pl_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            ((from db() in "platformlines" where ("pl_id" -> pl_id) select (x => x)).toList) match {
              case head :: Nil => {
                  (data \ "status").asOpt[Int].map { x =>
                      head += "status" -> x.asInstanceOf[Number]
                  }.getOrElse(Unit)
                 
                  _data_connection.getCollection("platformlines").update(DBObject("pl_id" -> pl_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def platformLineQuery(data : JsValue) : JsValue = {
        def basicCondition = "status" $gte lineStatus.pushed.t
      
        def conditionsAcc(o : DBObject, key : String, value : Any) : DBObject = {
            key match {
              case "status" => $and(o, "status" $eq value.asInstanceOf[Int])
              case "line" => {
                  val origin_province = (value.asInstanceOf[JsValue] \ "origin_province").asOpt[String].get
                  val origin_city = (value.asInstanceOf[JsValue] \ "origin_city").asOpt[String].get
                  val destination_province = (value.asInstanceOf[JsValue] \ "destination_province").asOpt[String].get
                  val destination_city = (value.asInstanceOf[JsValue] \ "destination_city").asOpt[String].get
                  
                  $and(o, $and(("line.origin_province" $eq origin_province) :: 
                               ("line.origin_city" $eq origin_city) :: 
                               ("line.destination_province" $eq destination_province) ::
                               ("line.destination_city" $eq destination_city) :: Nil))
              }
              case "vehicle" => {
                  val lst = value.asInstanceOf[List[String]]
                  val con = lst map { str =>
                      "detail.vehicle" $eq str
                  }
                  $and (o, $or(con))
              }
              case "vehicle_length" => {
                  val lst = value.asInstanceOf[List[Float]]
                  val con = lst map { f =>
                      "detail.vehicle_length" $eq f
                  }
                  $and (o, $or(con))
              }
              case "date" => {
                  val dt = value.asInstanceOf[JsValue]
                  val sdf = new java.text.SimpleDateFormat("MM/dd/yyyy")
                  val min = sdf.parse((dt \ "min").asOpt[String].get).getTime
                  val max = sdf.parse((dt \ "max").asOpt[String].get).getTime
                  
                  $and(o, $and("date" $gte min, "date" $lte max))
              }
            }
        }

        def conditions : DBObject = {
            var reVal : DBObject = basicCondition

            (data \ "status").asOpt[String].map (x => 
                reVal = conditionsAcc(reVal, "status", x)
            ).getOrElse(Unit)

            (data \ "date").asOpt[JsValue].map (x => 
                reVal = conditionsAcc(reVal, "date", x)
            ).getOrElse(Unit)
           
            (data \ "line").asOpt[JsValue].map { x =>
                reVal = conditionsAcc(reVal, "line", x)
            }.getOrElse(Unit)
            
            (data \ "vehicle").asOpt[List[String]].map { x =>
                reVal = conditionsAcc(reVal, "vehicle", x)
            }.getOrElse(Unit)

            (data \ "vehicle_length").asOpt[List[Float]].map { x =>
                reVal = conditionsAcc(reVal, "vehicle_length", x)
            }.getOrElse(Unit)
            
            reVal
        }
        
        def orderCol = "date"
      
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
        
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            ((from db() in "platformlines" where conditions).selectSkipTop(skip)(take)(orderCol) 
                (DB2JsValue(_))).toList)))
    }
    
    def DB2JsValue(x : MongoDBObject) : JsValue = {
        val date = Calendar.getInstance
        date.setTimeInMillis(x.getAs[Number]("date").get.longValue)
        val push_date = Calendar.getInstance
        push_date.setTimeInMillis(x.getAs[Number]("push_date").get.longValue)
    
        val line = x.getAs[MongoDBObject]("line").map { tmp => 
          toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").map (x => x).getOrElse(""),
                     "origin_city" -> tmp.getAs[String]("origin_city").map (x => x).getOrElse(""),
                     "destination_province" -> tmp.getAs[String]("destination_province").map (x => x).getOrElse(""),
                     "destination_city" -> tmp.getAs[String]("destination_city").map (x => x).getOrElse("")))
        }.getOrElse(throw new Exception("not exising"))
        
        toJson(Map("pl_id" -> toJson(x.getAs[String]("pl_id").get),
                   "notes" -> toJson(x.getAs[String]("notes").map (x => x).getOrElse("")),
                   "weight" -> toJson(x.getAs[Number]("weight").get.floatValue),
                   "volume" -> toJson(x.getAs[Number]("volume").get.floatValue),
                   "status" -> toJson(x.getAs[Number]("status").get.intValue),
                   "vehicle" -> toJson(x.getAs[List[String]]("vehicle").get),
                   "vehicle_length" -> toJson(x.getAs[List[Double]]("vehicle_length").get),
                   "line" -> line,
                   "date" -> toJson(Map("year" -> date.get(Calendar.YEAR),
                                        "month" -> (date.get(Calendar.MONTH) + 1),
                                        "day" -> date.get(Calendar.DAY_OF_MONTH))),
                   "push_date" -> toJson(Map("year" -> push_date.get(Calendar.YEAR),
                                        "month" -> (push_date.get(Calendar.MONTH) + 1),
                                        "day" -> push_date.get(Calendar.DAY_OF_MONTH)))))
    }
}