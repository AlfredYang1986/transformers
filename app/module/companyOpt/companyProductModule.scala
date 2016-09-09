package module.companyOpt

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
import module.sercurity.Sercurity

object productStatus {  
    case object published extends productStatusDefines(0, "approved")
    case object done extends productStatusDefines(1, "progress")
}

sealed abstract class productStatusDefines(val t : Int, val des : String)

object companyProductModule {
    def pushProduct(data : JsValue) : JsValue = 
        try {
            val builder = MongoDBObject.newBuilder
            
            (data \ "origin").asOpt[JsValue].map { x =>
                val origin_builder = MongoDBObject.newBuilder
                origin_builder += "province" -> (x \ "origin_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "city" -> (x \ "origin_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "district" -> (x \ "origin_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "address" -> (x \ "origin_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                builder += "origin" -> origin_builder.result
            }.getOrElse(throw new Exception("wrong input"))
            
            (data \ "destination").asOpt[JsValue].map { x =>
                val destination_builder = MongoDBObject.newBuilder
                destination_builder += "province" -> (x \ "destination_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "city" -> (x \ "destination_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "district" -> (x \ "destination_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "address" -> (x \ "destination_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                builder += "destination" -> destination_builder.result
            }.getOrElse(throw new Exception("wrong input"))
           
            builder += "vehicle" -> (data \ "vehicle").asOpt[List[String]].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "vehicle_length" -> (data \ "vehicle_length").asOpt[List[Float]].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "weight" -> (data \ "weight").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "volume" -> (data \ "volume").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            builder += "date_requirement" -> (data \ "date_requirement").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "notes" -> (data \ "notes").asOpt[String].map (x => x).getOrElse("")
            
            val seed = "alfred yang"
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "product_id" -> Sercurity.md5Hash(seed + Sercurity.getTimeSpanWithMillSeconds)
            builder += "open_id" -> open_id
            builder += "date" -> new Date().getTime
            builder += "status" -> productStatus.published.t

            val product_name = (data \ "product_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "product_name" -> product_name
            val product_name_save = (data \ "product_name_save").asOpt[Int].map (x => x).getOrElse(0)
            if (product_name_save == 1 && !product_name.equals("")) {
                companyConfigModule.companyConfigProductNamePush(toJson(Map("open_id" -> open_id, "pdn" -> product_name)))
            }
            
            val contact_name = (data \ "contact_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "contact_name" -> contact_name
            val contact_phone = (data \ "contact_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "contact_phone" -> contact_phone
            val contact_save = (data \ "contact_save").asOpt[Int].map (x => x).getOrElse(0)
            if (contact_save == 1 && !contact_name.equals("") && !contact_name.equals("")) {
                companyConfigModule.companyConfigContactPush(toJson(Map("open_id" -> open_id, "contact_name" -> contact_name, "contact_phone" -> contact_phone)))
            }
            
            _data_connection.getCollection("products") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def popProduct(data : JsValue) : JsValue = 
        try {
            val product_id = (data \ "product_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "products" where ("product_id" -> product_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("products") -= head
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("not existing")
            }
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def updateProduct(data : JsValue) : JsValue =
        try {
            val product_id = (data \ "product_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "products" where ("product_id" -> product_id) select (x => x)).toList match {
              case head :: Nil => {
                  (data \ "origin").asOpt[JsValue].map { x =>
                      val origin_builder = MongoDBObject.newBuilder
                      origin_builder += "province" -> (x \ "origin_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      origin_builder += "city" -> (x \ "origin_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      origin_builder += "district" -> (x \ "origin_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      origin_builder += "address" -> (x \ "origin_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      head += "origin" -> origin_builder.result
                  }.getOrElse(Unit)
                  
                  (data \ "destination").asOpt[JsValue].map { x =>
                      val destination_builder = MongoDBObject.newBuilder
                      destination_builder += "province" -> (x \ "destination_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      destination_builder += "city" -> (x \ "destination_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      destination_builder += "district" -> (x \ "destination_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      destination_builder += "address" -> (x \ "destination_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      head += "destination" -> destination_builder.result
                  }.getOrElse(Unit)
                 
                  (data \ "vehicle").asOpt[List[String]].map (x => head += "vehicle" -> x).getOrElse(Unit)
                  (data \ "vehicle_length").asOpt[List[Float]].map (x => head += "vehicle_length" -> x).getOrElse(Unit)
                  (data \ "weight").asOpt[Float].map (x => head += "weight" -> x.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "volume").asOpt[Float].map (x => head += "volume" -> x.asInstanceOf[Number]).getOrElse(Unit)
                  
                  (data \ "date_requirement").asOpt[String].map (x => head += "date_requirement" -> x).getOrElse(Unit)
                  (data \ "notes").asOpt[String].map (x => head += "notes" -> x).getOrElse(Unit)
                 
                  (data \ "status").asOpt[Int].map (x => head += "status" -> x.asInstanceOf[Number]).getOrElse(Unit)
      
                  val open_id = head.getAs[String]("open_id").get
                  val product_name = (data \ "product_name").asOpt[String].map (x => Some(x)).getOrElse(None)
                  product_name match {
                    case Some(x) => {
                        head += "product_name" -> x
                        val product_name_save = (data \ "product_name_save").asOpt[Int].map (x => x).getOrElse(0)
                        if (product_name_save == 1 && !x.equals("")) {
                            companyConfigModule.companyConfigProductNamePush(toJson(Map("open_id" -> open_id, "pdn" -> x)))
                        }
                    }
                    case None => Unit
                  }
                 
                  val contact_name = (data \ "contact_name").asOpt[String].map (x => Some(x)).getOrElse(None)
                  contact_name match {
                    case Some(x) => {
                        val contact_phone = (data \ "contact_phone").asOpt[String].map (x => Some(x)).getOrElse(None)
                        contact_phone match {
                          case Some(y) => {
                              val contact_save = (data \ "contact_save").asOpt[Int].map (x => x).getOrElse(0)
                              head += "contact_name" -> x
                              head += "contact_phone" -> y
                              if (contact_save == 1 && !x.equals("") && !y.equals("")) {
                                  companyConfigModule.companyConfigContactPush(toJson(Map("open_id" -> open_id, "contact_name" -> x, "contact_phone" -> y)))
                              }
                          }
                          case None => Unit
                        }
                    }
                    case None => Unit
                  }
                  
                  _data_connection.getCollection("products").update(DBObject("product_id" -> product_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("not existing")
            }
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def queryProduct(data : JsValue) : JsValue = {
        def addressConditions(getter : JsValue => Option[Any])(key : String, value : JsValue) : Option[DBObject] = getter(value) match {
          case None => None
          case Some(x) => {
            val tmp = x.asInstanceOf[JsValue]
            val province = (tmp \ "province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
            val city = (tmp \ "city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
            val district = (tmp \ "district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
            
            val con = $and($and((key + ".province") $eq province, (key + ".city") $eq city), (key + ".distinct") $eq district)
            (tmp \ "address").asOpt[String].map (x => Some($and(con, (key + ".address") $eq x))).getOrElse(Some(con)) 
          }
        }
        
        def stringConditions(getter : JsValue => Option[Any])(key : String, value : JsValue) : Option[DBObject] = getter(value) match {
          case None => None
          case Some(x) => Some(key $eq x.asInstanceOf[String])
        }
        
        def floatConditions(getter : JsValue => Any)(key : String, value : JsValue) : Option[DBObject] = getter(value) match {
          case None => None
          case Some(x) => Some(key $eq x.asInstanceOf[Float])
        }
          
        def intConditions(getter : JsValue => Any)(key : String, value : JsValue) : Option[DBObject] = getter(value) match {
          case None => None
          case Some(x) => Some(key $eq x.asInstanceOf[Int])
        }
        
        def ListConditions(key : String, value : String) : Option[DBObject] = {
            None
        }
        
        def conditionsOnce(o : Option[DBObject], n : Option[DBObject]) : Option[DBObject] = {
            o match {
              case None => n
              case Some(x) => {
                n match {
                  case None => o
                  case Some(y) => Some($and(x, y))
                }
              }
            }
        }
        
        def conditionsAcc(o : Option[DBObject], keys : List[String], func : (String, JsValue) => Option[DBObject]) : Option[DBObject] = keys match {
          case Nil => o
          case head :: lst => {
            val n = o match {
                        case None => func(head, (data \ head))
                        case Some(x) => {
                            func(head, (data \ head)) match {
                              case None => o
                              case Some(y) => Some($and(x, y))
                            }
                        }
                    }
            conditionsAcc(n, lst, func)
            }
        }
        
        def conditions : Option[DBObject] = {
            var con : Option[DBObject] = 
                conditionsAcc(None, "date_requirement" :: "open_id" :: "product_name" :: "product_id" :: "contact_name" :: "contact_phone" :: Nil, stringConditions(x => x.asOpt[String]))
            con = conditionsAcc(con, "weight" :: "volume" :: Nil, floatConditions(x => x.asOpt[Float]))
            con = conditionsAcc(con, "status" :: Nil, intConditions(x => x.asOpt[Int]))
            con = conditionsAcc(con, "origin" :: "destination" :: Nil, addressConditions(x => x.asOpt[Int]))

            con
        }
        
        try {
            conditions match {
              case None => 
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                      (from db() in "products" select (product2JsValue(_))).toList)))
              case Some(x) => 
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                      (from db() in "products" where x select (product2JsValue(_))).toList)))
            }
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def product2JsValue(x : MongoDBObject) : JsValue = {
        val origin = x.getAs[MongoDBObject]("origin").get
        val destination = x.getAs[MongoDBObject]("destination").get
        val date = Calendar.getInstance
        date.setTimeInMillis(x.getAs[Number]("date").get.longValue)
        toJson(Map("date_requirement" -> toJson(x.getAs[String]("date_requirement").get),
                   "open_id" -> toJson(x.getAs[String]("open_id").get),
                   "product_name" -> toJson(x.getAs[String]("product_name").get),
                   "product_id" -> toJson(x.getAs[String]("product_id").get),
                   "contact_name" -> toJson(x.getAs[String]("contact_name").get),
                   "contact_phone" -> toJson(x.getAs[String]("contact_phone").get),
                   "notes" -> toJson(x.getAs[String]("notes").get),
                   "weight" -> toJson(x.getAs[Number]("weight").get.floatValue),
                   "volume" -> toJson(x.getAs[Number]("volume").get.floatValue),
                   "vehicle" -> toJson(x.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                   "vehicle_length" -> toJson(x.getAs[MongoDBList]("vehicle_length").get.toList.asInstanceOf[List[Double]]),
                   "date" -> toJson(Map("year" -> date.get(Calendar.YEAR),
                                        "month" -> date.get(Calendar.MONTH),
                                        "day" -> date.get(Calendar.DAY_OF_MONTH))),
                   "status" -> toJson(x.getAs[Number]("status").get.intValue),
                   "origin" -> toJson(Map("province" -> toJson(origin.getAs[String]("province").get),
                                         "city" -> toJson(origin.getAs[String]("city").get),
                                         "district" -> toJson(origin.getAs[String]("district").get),
                                         "address" -> toJson(origin.getAs[String]("address").get))),
                   "destination" -> toJson(Map("province" -> toJson(destination.getAs[String]("province").get),
                                               "city" -> toJson(destination.getAs[String]("city").get),
                                               "district" -> toJson(destination.getAs[String]("district").get),
                                               "address" -> toJson(destination.getAs[String]("address").get)
                                               ))))
    }  
}