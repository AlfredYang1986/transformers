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

object companyConfigModule {
    def companyConfigProductNamePush(data : JsValue) : JsValue =
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pdn = (data \ "pdn").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => { 
                  val lst = head.getAs[MongoDBList]("pdn").get.toList.asInstanceOf[List[String]]
                  head += "pdn" -> (pdn :: lst).distinct
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              } 
              case Nil => {
                  val builder = MongoDBObject.newBuilder
                  builder += "open_id" -> open_id
                  builder += "pdn" -> (pdn :: Nil)
                  builder += "contacts" -> MongoDBList.newBuilder.result
                  _data_connection.getCollection("company_config") += builder.result
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def companyConfigProductNamePop(data : JsValue) : JsValue =
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pdn = (data \ "pdn").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => { 
                  val lst = head.getAs[MongoDBList]("pdn").get.toList.asInstanceOf[List[String]]
                  head += "pdn" -> lst.filterNot (x => pdn.equals(x))
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }

    def companyConfigProductNameUpdate(data : JsValue) : JsValue =
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pdn = (data \ "pdn").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val old = (data \ "old").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => { 
                  val lst = head.getAs[MongoDBList]("pdn").get.toList.asInstanceOf[List[String]]
                  head += "pdn" -> (pdn :: lst.filterNot (x => old.equals(x))).distinct
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def companyConfigProductNameQuery(data : JsValue) : JsValue =
        try {
            (from db() in "company_config" where ("open_id" -> 
                (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))) 
                    select (x => x.getAs[MongoDBList]("pdn").get.toList.asInstanceOf[List[String]])).toList match {
              case head :: Nil => toJson(Map("status" -> toJson("ok"), "result" -> toJson(head)))   
              case _ => throw new Exception("new existing")
            }
           
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def companyConfigContactPush(data : JsValue) : JsValue = 
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_name = (data \ "contact_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_phone = (data \ "contact_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
         
            val contact_id = Sercurity.md5Hash(contact_name + contact_phone + Sercurity.getTimeSpanWithMillSeconds)

            val contact_builder = MongoDBObject.newBuilder
            contact_builder += "contact_id" -> contact_id
            contact_builder += "contact_name" -> contact_name
            contact_builder += "contact_phone" -> contact_phone
           
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => { 
                  val lst = head.getAs[MongoDBList]("contacts").get.toList.asInstanceOf[List[BasicDBObject]]
                  head += "contacts" -> (contact_builder.result :: lst)
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              } 
              case Nil => {
                  val builder = MongoDBObject.newBuilder
                  builder += "open_id" -> open_id
                  builder += "pdn" -> MongoDBList.newBuilder.result
                  builder += "contacts" -> (contact_builder.result :: Nil)
                  _data_connection.getCollection("company_config") += builder.result
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def companyConfigContactPop(data : JsValue) : JsValue = 
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_id = (data \ "contact_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val lst = head.getAs[MongoDBList]("contacts").get.toList.asInstanceOf[List[BasicDBObject]]
                  head += "contacts" -> lst.filterNot (x => contact_id.equals(x.getAs[String]("contact_id").get))
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def companyConfigContactUpdate(data : JsValue) : JsValue = 
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_id = (data \ "contact_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_name = (data \ "contact_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_phone = (data \ "contact_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val lst = head.getAs[MongoDBList]("contacts").get.toList.asInstanceOf[List[BasicDBObject]]
                  
                  val contact_builder = MongoDBObject.newBuilder
                  contact_builder += "contact_id" -> contact_id
                  contact_builder += "contact_name" -> contact_name
                  contact_builder += "contact_phone" -> contact_phone
               
                  head += "contacts" -> (contact_builder.result :: lst.filterNot (x => contact_id.equals(x.getAs[String]("contact_id").get)))
                  _data_connection.getCollection("company_config").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success"))) 
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def companyConfigContactQuery(data : JsValue) : JsValue = {
        def contact2JsValue(x : MongoDBObject) : JsValue = {
            toJson(Map("contact_id" -> toJson(x.getAs[String]("contact_id").get),
                       "contact_name" -> toJson(x.getAs[String]("contact_name").get),
                       "contact_phone" -> toJson(x.getAs[String]("contact_phone").get)))
        }
      
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
         
            (from db() in "company_config" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(head.getAs[MongoDBList]("contacts").get.toList.asInstanceOf[List[BasicDBObject]] map (contact2JsValue(_)))))
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => println(ex); ErrorCode.errorToJson(ex.getMessage)
        }
    }
}