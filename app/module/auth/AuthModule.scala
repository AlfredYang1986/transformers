package module.auth

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

object authStatus {  // auth  indicate weather account is approved 
    case object approved extends authTypeDefines(2, "approved")
    case object progress extends authTypeDefines(1, "progress")

    case object statusBase extends authTypeDefines(0, "status base")

    case object rejected extends authTypeDefines(-1, "rejected")

    case object anyStatus extends authTypeDefines(-99, "any status")
}

sealed abstract class authStatusDefines(val t : Int, val des : String)

object authTypes {  // auth  indicate which account 
    case object anyBody extends authTypeDefines(-99, "any body")
    case object notAuth extends authTypeDefines(-1, "not auth")

    case object driverBase extends authTypeDefines(0, "driver base")
    case object driverMaster extends authTypeDefines(1, "driver master")

    case object companyBase extends authTypeDefines(10, "company base")
    case object companyOthers extends authTypeDefines(11, "company others")
    case object companyMaster extends authTypeDefines(12, "company master")
    
    case object speicalwayBase extends authTypeDefines(20, "speical base")
    case object speicalwayOther extends authTypeDefines(21, "speical others")
    case object speicalwayMaster extends authTypeDefines(22, "speical master")
    
    case object adminBase extends authTypeDefines(100, "admin base")
    case object adminAdjusted extends authTypeDefines(101, "adminAdjusted")
    case object adminSendCar extends authTypeDefines(102, "adminSendCar")
    case object adminMaster extends authTypeDefines(110, "admin master")
}

sealed abstract class authTypeDefines(val t : Int, val des : String)

object registerTypes {  // type indicate type of account
    case object admin extends registerTypesDefines(-1, "admin")
    case object driver extends registerTypesDefines(0, "driver")
    case object company extends registerTypesDefines(1, "company")
    case object industry extends registerTypesDefines(2, "industry")
    case object specialway extends registerTypesDefines(3, "specialway")
}

sealed abstract class registerTypesDefines(val t : Int, val des : String)

object businessTypes {  // company business indicate of company business
    case object car extends businessTypesDefines(0, "公路")
    case object train extends businessTypesDefines(1, "铁路")
    case object ship extends businessTypesDefines(2, "船运")
    case object plane extends businessTypesDefines(3, "航空")
}

sealed abstract class businessTypesDefines(val t : Int, val des : String)

object occationStatus {  // driver insurance indicate of driver insurance
    case object everyday extends occationStatusDefines(0, "每天发车")
    case object everyotherday extends occationStatusDefines(1, "隔天发车")
}

sealed abstract class occationStatusDefines(val t : Int, val des : String)

object insuranceStatus {  // driver insurance indicate of driver insurance
    case object insuranced extends insuranceStatusDefines(0, "已购买")
    case object not_insuranced extends insuranceStatusDefines(1, "未购买")
}

sealed abstract class insuranceStatusDefines(val t : Int, val des : String)

object AuthModule {
 
    def indicateValidateCheck(data : JsValue) : JsValue = {
        
        try {
            val company_type = (data \ "company_type").asOpt[Int].map (x => x).getOrElse(throw new Exception("Bad Input"))
   
            val indicate = company_type match {
              case registerTypes.driver.t => (data \ "phone_no").asOpt[String].map (x => x).getOrElse(throw new Exception("input driver phone"))
              case registerTypes.company.t | registerTypes.industry.t | registerTypes.specialway.t => 
                    (data \ "cell_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("input driver phone"))
              case registerTypes.admin.t => 
                    (data \ "phone").asOpt[String].map (x => x).getOrElse(throw new Exception("input driver phone"))
            }
            
            (from db() in "user_profile" where ("user_lst.indicate" -> indicate) select (x => x)).toList match {
                case Nil => toJson(Map("status" -> "ok", "result"-> "success"))
                case _ => throw new Exception("duplicate phone or email")
            }
          
        } catch {
          
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
  
    def regCodeCheck(data : JsValue) : JsValue = {
        try {
            val cell_phone = (data \ "cell_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong cell phone"))
            val code = (data \ "code").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong code"))
            val validate = Sercurity.getTimeSpanWith10Minutes
            
            (from db() in "reg" where ("cell_phone" -> cell_phone) select (x => x)).toList match {
              case Nil => throw new Exception("wrong cell phone")
              case head :: Nil => {
                  if (head.getAs[String]("code").get != code) throw new Exception("wrong code")
                  else if (head.getAs[String]("validate").get != validate) throw new Exception("not validate code")
                  else toJson(Map("status" -> "ok", "result" -> "code is validate"))
              }
              case _ => throw new Exception("wrong cell phone")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
  
    def register(data : JsValue) : JsValue = {
        val company_type = (data \ "company_type").asOpt[Int].map (x => x).getOrElse(throw new Exception("Bad Input"))

        def commonRegisterImpl(x : MongoDBObject) : (Boolean, String) =
            try {
                val company_name = (data \ "company_name").asOpt[String].map (tmp => tmp).getOrElse(throw new Exception("input company name"))
                x += "company_name" -> company_name
//                (data \ "company_name").asOpt[String].map (tmp => x += "company_name" -> tmp).getOrElse(throw new Exception("input company name"))
                (data \ "legal_person").asOpt[String].map {tmp => x += "legal_person" -> tmp; x += "cell_phone_owner" -> tmp}.getOrElse(throw new Exception("input legal person"))
                (data \ "legal_person_id").asOpt[String].map (tmp => x += "legal_person_id" -> tmp).getOrElse(throw new Exception("input legal person id"))
                (data \ "address").asOpt[String].map (tmp => x += "address" -> tmp).getOrElse(throw new Exception("input company reg address"))
                (data \ "phone_dir").asOpt[String].map (tmp => x += "phone_dir" -> tmp).getOrElse(x += "phone_dir" -> "")
                (data \ "phone_no").asOpt[String].map (tmp => x += "phone_no" -> tmp).getOrElse(x += "phone_no" -> "")
                (data \ "phone_sep").asOpt[String].map (tmp => x += "phone_sep" -> tmp).getOrElse(x += "phone_sep" -> "")
                (data \ "business_image").asOpt[String].map (tmp => x += "business_image" -> tmp).getOrElse(throw new Exception("input business image"))
//                (data \ "road_image").asOpt[String].map (tmp => x += "road_image" -> tmp).getOrElse(throw new Exception("input road image"))
                (data \ "road_image").asOpt[String].map (tmp => x += "road_image" -> tmp).getOrElse(x += "road_image" -> "")
                (data \ "cell_phone").asOpt[String].map (tmp => x += "cell_phone" -> tmp).getOrElse(throw new Exception("wrong cell phone"))
//                (data \ "cell_phone_owner").asOpt[String].map (tmp => x += "cell_phone_owner" -> tmp).getOrElse(throw new Exception("input legal person"))
                x += "type" -> company_type.asInstanceOf[Number]
                x += "auth_status" -> authStatus.progress.t.asInstanceOf[Number]
                x += "open_id" -> module.sercurity.Sercurity.md5Hash(company_name + Sercurity.getTimeSpanWithMillSeconds)
                x += "date" -> new Date().getTime.asInstanceOf[Number]
                
                (true, "")
            } catch {
              case ex : Exception => (false, ex.getMessage)
            }
      
        def companyRegisterImpl(x : MongoDBObject) : (Boolean, String) = {
            try {
                val lines = MongoDBList.newBuilder
                (data \ "company_lines").asOpt[List[JsValue]].getOrElse(Nil).foreach { iter => 
                      val line = MongoDBObject.newBuilder
                      (iter \ "origin_province").asOpt[String].map (tmp => line += "origin_province" -> tmp).getOrElse(line += "origin_province" -> "")
                      (iter \ "origin_city").asOpt[String].map (tmp => line += "origin_city" -> tmp).getOrElse(line += "origin_city" -> "")
                      (iter \ "destination_province").asOpt[String].map (tmp => line += "destination_province" -> tmp).getOrElse(line += "destination_province" -> "")
                      (iter \ "destination_city").asOpt[String].map (tmp => line += "destination_city" -> tmp).getOrElse(line += "destination_city" -> "")
                      lines += line.result
                }
                x += "company_lines" -> lines.result
             
                val business_field = MongoDBList.newBuilder
                (data \ "company_business").asOpt[List[String]].map { lst => lst.map { iter =>
                    business_field += iter
                }}.getOrElse(throw new Exception("input company business"))
                x += "company_business" -> business_field.result
                
//                (data \ "company_business").asOpt[Int].map (tmp => x += "company_business" -> tmp.asInstanceOf[Number]).getOrElse(throw new Exception("input company business"))
                (data \ "company_web").asOpt[String].map (tmp => x += "company_web" -> tmp).getOrElse("")
                (data \ "company_fax").asOpt[String].map (tmp => x += "company_fax" -> tmp).getOrElse("")
                (data \ "company_email").asOpt[String].map (tmp => x += "company_email" -> tmp).getOrElse(throw new Exception("input company email"))
                
                (true, "")
            } catch {
              case ex : Exception => (false, ex.getMessage)
            }
        }
        
        def industryRegisterImpl(x : MongoDBObject) : (Boolean, String) = {
            try {
                (data \ "industry_web").asOpt[String].map (tmp => x += "industry_web" -> tmp).getOrElse(x += "industry_web" -> "")
                (data \ "industry_fax").asOpt[String].map (tmp => x += "industry_fax" -> tmp).getOrElse(x += "industry_fax" -> "")
                (data \ "industry_email").asOpt[String].map (tmp => x += "industry_email" -> tmp).getOrElse(throw new Exception("input company email"))
                
                (true, "")
            } catch {
              case ex : Exception => (false, ex.getMessage)
            }
        }
        
        def spicalwayRegisterImpl(x : MongoDBObject) : (Boolean, String) = {
            try {
                val lst = MongoDBList.newBuilder
                (data \ "special_storage").asOpt[List[JsValue]].getOrElse(Nil).foreach { iter => 
                      val storage = MongoDBObject.newBuilder
                      (iter \ "province").asOpt[String].map (tmp => storage += "province" -> tmp).getOrElse(storage += "provice" -> "")
                      (iter \ "city").asOpt[String].map (tmp => storage += "city" -> tmp).getOrElse(storage += "city" -> "")
                      (iter \ "district").asOpt[String].map (tmp => storage += "district" -> tmp).getOrElse(storage += "district" -> "")
                      (iter \ "address").asOpt[String].map (tmp => storage += "address" -> tmp).getOrElse(storage += "address" -> "")
                      lst += storage.result
                }
                x += "special_storage" -> lst.result
                
                val lines = MongoDBList.newBuilder
                (data \ "special_lines").asOpt[List[JsValue]].getOrElse(Nil).foreach { iter => 
                      val line = MongoDBObject.newBuilder
                      (iter \ "origin_province").asOpt[String].map (tmp => line += "origin_province" -> tmp).getOrElse(line += "origin_province" -> "")
                      (iter \ "origin_city").asOpt[String].map (tmp => line += "origin_city" -> tmp).getOrElse(line += "origin_city" -> "")
                      (iter \ "destination_province").asOpt[String].map (tmp => line += "destination_province" -> tmp).getOrElse(line += "destination_province" -> "")
                      (iter \ "destination_city").asOpt[String].map (tmp => line += "destination_city" -> tmp).getOrElse(line += "destination_city" -> "")
                      lines += line.result
                }
                x += "special_lines" -> lines.result
                
                val vehicle = MongoDBList.newBuilder
                (data \ "vehicle").asOpt[List[String]].getOrElse(Nil).foreach { iter =>
                      vehicle += iter
                }
                x += "vehicle" -> vehicle.result 
                
                (data \ "special_occation").asOpt[Int].map (tmp => x += "special_occation" -> tmp.asInstanceOf[Number]).getOrElse("special_occation" -> occationStatus.everyday.t)
                (data \ "special_web").asOpt[String].map (tmp => x += "special_web" -> tmp).getOrElse("special_web" -> "")
                (data \ "special_fax").asOpt[String].map (tmp => x += "special_fax" -> tmp).getOrElse("special_web" -> "")
                (data \ "special_email").asOpt[String].map (tmp => x += "special_email" -> tmp).getOrElse(throw new Exception("input company email"))
                
                (true, "")
            } catch {
              case ex : Exception => (false, ex.getMessage)
            }
        }
        
        import registerTypes._
        def createBasicAccount(x : MongoDBObject, company_type : Int) = {
            val name = (data \ "company_name").asOpt[String].get
            val email = (data \ (company_type match {
              case company.t => "company_email"
              case industry.t => "industry_email"
              case specialway.t => "special_email"
            })).asOpt[String].map (x => x).getOrElse("")

            val user_id = Sercurity.md5Hash(name + email + Sercurity.getTimeSpanWithMillSeconds)
            x += "user_id" -> user_id
            x += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
            x += "auth" -> (if (company_type == specialway.t) authTypes.speicalwayMaster.t.asInstanceOf[Number]
                           else authTypes.companyMaster.t.asInstanceOf[Number])
            x += "indicate" -> name
            x += "pwd" -> "Passw0rd"
            x += "screen_name" -> "company master"
        }
      
        val v_indicate = indicateValidateCheck(data)
//        val v_reg = regCodeCheck(data)
        
        if ((v_indicate \ "status").asOpt[String].get == "error") v_indicate
//        else if ((v_reg \ "status").asOpt[String].get == "error") v_reg
        else {
          val common = MongoDBObject.newBuilder.result
          val (common_result, common_error) = commonRegisterImpl(common)
          if (!common_result) ErrorCode.errorToJson(common_error)
          else {
              val detail = MongoDBObject.newBuilder.result
              val (detail_result, detail_error) = company_type match {
                case company.t => companyRegisterImpl(detail)
                case industry.t => industryRegisterImpl(detail)
                case specialway.t => spicalwayRegisterImpl(detail)
                case _ => ???
              }
              
              if (!detail_result) ErrorCode.errorToJson(detail_error)
              else {
                 common += "detail" -> detail
                 val user_lst = MongoDBList.newBuilder
                 val company_master = MongoDBObject.newBuilder.result
                 createBasicAccount(company_master, company_type)
                 user_lst += company_master
                 common += "user_lst" -> user_lst.result
                 
                 _data_connection.getCollection("user_profile") += common
                 toJson(Map("status" -> toJson("ok"), "result" -> userResult(company_master)))
              }
          }
        }
    }
    
    def driverRegister(data : JsValue) : JsValue = {
      
        import registerTypes._
        def createBasicAccount(x : MongoDBObject) = {
            val name = (data \ "driver_name").asOpt[String].get
            val phone = (data \ "phone_no").asOpt[String].get

            val user_id = Sercurity.md5Hash(name + phone + Sercurity.getTimeSpanWithMillSeconds)
            x += "user_id" -> user_id
            x += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
            x += "auth" -> authTypes.driverMaster.t.asInstanceOf[Number]
            x += "indicate" -> phone
            x += "pwd" -> "Passw0rd"
            x += "screen_name" -> name
        }
      
        try {
            val v_indicate = indicateValidateCheck(data)
//            val v_reg = regCodeCheck(data)
            
            if ((v_indicate \ "status").asOpt[String].get == "error") v_indicate
//            else if ((v_reg \ "status").asOpt[String].get == "error") v_reg
            else {
                val x = MongoDBObject.newBuilder
                val vehicle = MongoDBList.newBuilder
                (data \ "vehicle").asOpt[List[String]].getOrElse(Nil).foreach { iter =>
                      vehicle += iter
                }
                x += "vehicle" -> vehicle.result 
                
                val lines = MongoDBList.newBuilder
                (data \ "driver_lines").asOpt[List[JsValue]].getOrElse(Nil).foreach { iter => 
                      val line = MongoDBObject.newBuilder
                      (iter \ "origin_province").asOpt[String].map (tmp => line += "origin_province" -> tmp).getOrElse(line += "origin_province" -> "")
                      (iter \ "origin_city").asOpt[String].map (tmp => line += "origin_city" -> tmp).getOrElse(line += "origin_city" -> "")
                      (iter \ "destination_province").asOpt[String].map (tmp => line += "destination_province" -> tmp).getOrElse(line += "destination_province" -> "")
                      (iter \ "destination_city").asOpt[String].map (tmp => line += "destination_city" -> tmp).getOrElse(line += "destination_city" -> "")
                      lines += line.result
                }
                x += "driver_lines" -> lines.result
               
                import insuranceStatus._
                val driver_secial_id = (data \ "driver_secial_id").asOpt[String].map (tmp => tmp).getOrElse(throw new Exception("input driver secial id"))
                x += "driver_secial_id" -> driver_secial_id
                (data \ "driver_name").asOpt[String].map (tmp => x += "driver_name" -> tmp).getOrElse(throw new Exception("input driver name"))
    //            (data \ "driver_secial_id").asOpt[String].map (tmp => x += "secial_id" -> tmp).getOrElse(throw new Exception("input driver secial id"))
                (data \ "phone_no").asOpt[String].map (tmp => x += "phone_no" -> tmp).getOrElse(throw new Exception("input driver phone"))
                (data \ "vehicle_length").asOpt[List[Float]].map (tmp => x += "vehicle_length" -> tmp).getOrElse(x += "vehicle_length" -> MongoDBList.newBuilder.result)
//                (data \ "insurance").asOpt[Int].map (tmp => x += "insurance" -> tmp.asInstanceOf[Number]).getOrElse(x += "insurance" -> not_insuranced.t)
//                (data \ "capacity").asOpt[Int].map (tmp => x += "capacity" -> tmp.asInstanceOf[Number]).getOrElse(x += "capacity" -> 0)
                (data \ "driver_image").asOpt[String].map (tmp => x += "driver_image" -> tmp).getOrElse(throw new Exception("input drive image"))
                (data \ "road_image").asOpt[String].map (tmp => x += "road_image" -> tmp).getOrElse(throw new Exception("input drive road image"))
                x += "auth_status" -> authStatus.progress.t.asInstanceOf[Number]
                x += "open_id" -> Sercurity.md5Hash(driver_secial_id + Sercurity.getTimeSpanWithMillSeconds)
                x += "type" -> registerTypes.driver.t.asInstanceOf[Number]
                x += "date" -> new Date().getTime.asInstanceOf[Number]
               
                val user_lst = MongoDBList.newBuilder
                val company_master = MongoDBObject.newBuilder.result
                createBasicAccount(company_master)
                user_lst += company_master
                x += "user_lst" -> user_lst.result
                
                _data_connection.getCollection("user_profile") += x.result
        
                toJson(Map("status" -> toJson("ok"), "result" -> userResult(company_master)))
            }
        } catch {
            case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryUserLstWithOpenID(open_id : String) : List[JsValue] =
        ((from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
            case head :: Nil => userlst(head)
            case Nil => Nil 
            case _ => ???
        })
  
    def userlst(x : MongoDBObject) : List[JsValue] = {
//        val user_lst = x.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]].filter (x => (x.get("auth").asInstanceOf[Number].intValue) < authTypes.companyMaster.t)
        val user_lst = x.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]//.filter (x => (x.get("auth").asInstanceOf[Number].intValue) < authTypes.companyMaster.t)
        user_lst map { x => 
            toJson(Map("screen_name" -> toJson(x.getAs[String]("screen_name").get),
                       "user_id" -> toJson(x.getAs[String]("user_id").get),
                       "token" -> toJson(x.getAs[String]("token").get),
                       "social_id" -> toJson(x.getAs[String]("social_id").map (y => y).getOrElse("")),
                       "auth" -> toJson(x.getAs[Number]("auth").map (y => y.intValue).getOrElse(0)),
                       "phone" -> toJson(x.getAs[String]("indicate").map (y => y).getOrElse(""))))}
    }
    
    def pushSubuser(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val indicate = (data \ "phone").asOpt[String].map(x => x).getOrElse(throw new Exception("wrong input"))
            val name = (data \ "screen_name").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
       
            val validate = indicateValidateCheck(toJson(Map("company_type" -> toJson(-1), "phone" -> toJson(indicate))))
            (validate \ "status").asOpt[String].map { x => x match { 
              case "ok" => Unit 
              case _ => throw new Exception("duplicate phone or email")
            }}
            
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                 val user_lst = head.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]
                 user_lst.filter (x => indicate.equals(x.get("indicate"))) match  {
                   case Nil => {
                       val builder = MongoDBObject.newBuilder
                       val user_id = Sercurity.md5Hash(name + Sercurity.getTimeSpanWithMillSeconds)
                       builder += "indicate" -> indicate
                       builder += "user_id" -> user_id
                       builder += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
                       builder += "screen_name" -> name
                       builder += "auth" -> (data \ "auth").asOpt[Int].map (y => y).getOrElse(authTypes.companyOthers.t)
                       builder += "pwd" -> (data \ "pwd").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                       builder += "social_id" -> (data \ "social_id").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                       
                       head += "user_lst" -> (builder.result :: user_lst)
                       _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                       toJson(Map("status" -> "ok", "result" -> "success"))
                   }
                   case _ => throw new Exception("user existing")
                 }
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def popSubuser(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
//            val token = (data \ "token").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                 val user_lst = head.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]
                 head += "user_lst" -> user_lst.filterNot (x => user_id.equals(x.get("user_id")))
                 _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                 toJson(Map("status" -> "ok", "result" -> "success"))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => throw new Exception(ex.getMessage)
        }
    }
    
    def updateSubuser(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                 val user_lst = head.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]
                 user_lst.filter (x => user_id.equals(x.get("user_id"))) match  {
                   case user :: Nil => {
                       (data \ "screen_name").asOpt[String].map (x => user += "screen_name" -> x).getOrElse(Unit)
                       (data \ "phone").asOpt[String].map (x => user += "indicate" -> x).getOrElse(Unit)
                       (data \ "social_id").asOpt[String].map (x => user += "social_id" -> x).getOrElse(Unit)
                    
                       head += "user_lst" -> (user :: (user_lst.filterNot (x => user_id.equals(x.get("user_id")))))
                       _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                       toJson(Map("status" -> "ok", "result" -> "success"))
                   }
                   case _ => throw new Exception("user existing")
                 }
              }
              case _ => throw new Exception("not existing")
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def userResult(x : MongoDBObject) : JsValue =
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "token" -> toJson(x.getAs[String]("token").get),
                   "auth" -> toJson(x.getAs[Number]("auth").get.intValue),
                   "screen_name" -> toJson(x.getAs[String]("screen_name").get)))
    
    def detailResult(x : MongoDBObject) : JsValue =
        toJson(x.getAs[Number]("type").get.intValue match {
            case registerTypes.driver.t => Map(
                "open_id" -> toJson(x.getAs[String]("open_id").get),
                "driver_name" -> toJson(x.getAs[String]("driver_name").get),
                "driver_secial_id" -> toJson(x.getAs[String]("driver_secial_id").get),
                "type" -> toJson(x.getAs[Number]("type").get.intValue),
                "date" -> toJson(x.getAs[Number]("date").get.longValue),
                "capacity" -> toJson(x.getAs[Number]("capacity").map (x => x.floatValue).getOrElse(0.floatValue)),
                "vehicle_length" -> x.getAs[List[Number]]("vehicle_length").map (x => toJson(x.toList.map (y => y.floatValue))).getOrElse(toJson(x.getAs[Number]("vehicle_length").get.floatValue)),
                "insurance" -> toJson(x.getAs[Number]("insurance").map (x => x.intValue).getOrElse(0.intValue)),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "driver_image" -> toJson(x.getAs[String]("driver_image").get),
                "road_image" -> toJson(x.getAs[String]("road_image").map (x => x).getOrElse("")),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "vehicle" -> toJson(x.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                "driver_lines" -> toJson(x.getAs[MongoDBList]("driver_lines").get.toList.asInstanceOf[List[BasicDBObject]].map (tmp => 
                    toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                               "origin_city" -> tmp.getAs[String]("origin_city").get,
                               "destination_province" -> tmp.getAs[String]("destination_province").get,
                               "destination_city" -> tmp.getAs[String]("destination_city").get)))))
            case registerTypes.company.t => Map(
                "open_id" -> toJson(x.getAs[String]("open_id").get),
                "company_name" -> toJson(x.getAs[String]("company_name").get),
                "type" -> toJson(x.getAs[Number]("type").get.intValue),
                "date" -> toJson(x.getAs[Number]("date").get.longValue),
                "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                "legal_person_id" -> toJson(x.getAs[String]("legal_person_id").get),
                "address" -> toJson(x.getAs[String]("address").get),
                "business_image" -> toJson(x.getAs[String]("business_image").get),
                "road_image" -> toJson(x.getAs[String]("road_image").map (x => x).getOrElse("")),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                "cell_phone" -> toJson(x.getAs[String]("cell_phone").map (x => x).getOrElse("")),
                "cell_phone_owner" -> toJson(x.getAs[String]("cell_phone_owner").map (x => x).getOrElse("")),
                "description" -> toJson(x.getAs[String]("description").map (x => x).getOrElse("")),
                
                "company_business" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("company_business").get.toList.asInstanceOf[List[String]]),
                "company_web" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("company_web").get),
                "company_fax" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("company_fax").get),
                "company_email" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("company_email").get),
                "company_lines" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("company_lines").get.toList.asInstanceOf[List[BasicDBObject]].map (tmp =>
                    toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                               "origin_city" -> tmp.getAs[String]("origin_city").get,
                               "destination_province" -> tmp.getAs[String]("destination_province").get,
                               "destination_city" -> tmp.getAs[String]("destination_city").get)))))
            case registerTypes.industry.t => Map(
                "open_id" -> toJson(x.getAs[String]("open_id").get),
                "company_name" -> toJson(x.getAs[String]("company_name").get),
                "type" -> toJson(x.getAs[Number]("type").get.intValue),
                "date" -> toJson(x.getAs[Number]("date").get.longValue),
                "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                "legal_person_id" -> toJson(x.getAs[String]("legal_person_id").get),
                "address" -> toJson(x.getAs[String]("address").get),
                "business_image" -> toJson(x.getAs[String]("business_image").get),
                "road_image" -> toJson(x.getAs[String]("road_image").map (x => x).getOrElse("")),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                "cell_phone" -> toJson(x.getAs[String]("cell_phone").map (x => x).getOrElse("")),
                "cell_phone_owner" -> toJson(x.getAs[String]("cell_phone_owner").map (x => x).getOrElse("")),
                "description" -> toJson(x.getAs[String]("description").map (x => x).getOrElse("")),
                
                "industry_web" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("industry_web").get),
                "industry_fax" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("industry_fax").get),
                "industry_email" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("industry_email").get)
            )
            case registerTypes.specialway.t => Map(
                "open_id" -> toJson(x.getAs[String]("open_id").get),
                "company_name" -> toJson(x.getAs[String]("company_name").get),
                "type" -> toJson(x.getAs[Number]("type").get.intValue),
                "date" -> toJson(x.getAs[Number]("date").get.longValue),
                "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                "legal_person_id" -> toJson(x.getAs[String]("legal_person_id").get),
                "address" -> toJson(x.getAs[String]("address").get),
                "business_image" -> toJson(x.getAs[String]("business_image").get),
                "road_image" -> toJson(x.getAs[String]("road_image").map (x => x).getOrElse("")),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                "cell_phone" -> toJson(x.getAs[String]("cell_phone").map (x => x).getOrElse("")),
                "cell_phone_owner" -> toJson(x.getAs[String]("cell_phone_owner").map (x => x).getOrElse("")),
                "description" -> toJson(x.getAs[String]("description").map (x => x).getOrElse("")),
                
                "vehicle" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                "special_occation" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[Number]("special_occation").map (x => x.intValue).getOrElse(occationStatus.everyday.t)),
                "special_web" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_web").get),
                "special_fax" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_fax").get),
                "special_email" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_email").get),
                "special_storage" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("special_storage").get.toList.asInstanceOf[List[BasicDBObject]].map (tmp => 
                    toJson(Map("province" -> tmp.getAs[String]("province").get,
                               "city" -> tmp.getAs[String]("city").get,
                               "district" -> tmp.getAs[String]("district").map (x => x).getOrElse(""),
                               "address" -> tmp.getAs[String]("address").get)))),
                "special_lines" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("special_lines").get.toList.asInstanceOf[List[BasicDBObject]].map (tmp => 
                    toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                               "origin_city" -> tmp.getAs[String]("origin_city").get,
                               "destination_province" -> tmp.getAs[String]("destination_province").get,
                               "destination_city" -> tmp.getAs[String]("destination_city").get)))))
            case _ => ???
         })
   
    def adminMasterCreate = {
        val seed = "Alfred Yang"
        val admin = "admin"
      
        val admin_builder = MongoDBObject.newBuilder
        admin_builder += "open_id" -> Sercurity.md5Hash(admin + seed + Sercurity.getTimeSpanWithMillSeconds)
        admin_builder += "auth_status" -> authStatus.approved.t
        admin_builder += "type" -> registerTypes.admin.t
       
        val admin_master = MongoDBObject.newBuilder
        val user_id = Sercurity.md5Hash(admin + Sercurity.getTimeSpanWithMillSeconds)
        admin_master += "user_id" -> user_id
        admin_master += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
        admin_master += "auth" -> authTypes.adminMaster.t
        admin_master += "indicate" -> admin
        admin_master += "pwd" -> "Passw0rd"
        admin_master += "screen_name" -> admin
        
        val admin_user_lst = MongoDBList.newBuilder
        admin_user_lst += admin_master.result
        
        admin_builder += "user_lst" -> admin_user_lst.result
        
        _data_connection.getCollection("user_profile") += admin_builder.result
    }
                   
//    def login(open_id : String, user_id: String, data : JsValue) : JsValue = {
    def login(data : JsValue) : JsValue = {
        val indicate = (data \ "indicate").asOpt[String].map (x => x).getOrElse("")
        val pwd = (data \ "pwd").asOpt[String].map (x => x).getOrElse("")
     
        if (indicate == "" || pwd == "") ErrorCode.errorToJson("error input")
        else {
            try {
                toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                    ((from db() in "user_profile" where ("user_lst" $elemMatch($and("indicate" $eq indicate, "pwd" $eq pwd))) 
                        select (x => x.getAs[MongoDBList]("user_lst").get)).toList.flatten.filter
                            (x => x.asInstanceOf[BasicDBObject].get("indicate") == indicate && x.asInstanceOf[BasicDBObject].get("pwd") == pwd)) match {
                      case Nil => throw new Exception("user not exist")
                      case head :: Nil => toJson(userResult(head.asInstanceOf[BasicDBObject]))
                      case _ => ???
                })))
            } catch {
              case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
            }
        }
    }
    
//    def adminLogin(open_id : String, user_id : String, data : JsValue) : JsValue = {
    def adminLogin(data : JsValue) : JsValue = {
        val result = login(data)
        if ((result \ "result" \ "auth").asOpt[Int].map (x => x).getOrElse(authTypes.anyBody.t) > authTypes.adminBase.t) result
        else ErrorCode.errorToJson("user not exist")
    }
    
//    def queryProfile(open_id : String, user_id : String, data : JsValue) : JsValue = {
    def queryProfile(open_id : String) : JsValue = {
        toJson((from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("error input")
                  case head :: Nil => detailResult(head)
                  case _ => ???
               })
    }
    
    def queryProfileCondition(open_id : String, user_id : String, data : JsValue) : JsValue = {
      
        try {
            val kt = (data \ "type").asOpt[Int].map (x => x).getOrElse(throw new Exception("error input"))
            val st = (data \ "auth_status").asOpt[Int].map (x => x).getOrElse(throw new Exception("error input"))
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
         
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
              (from db() in "user_profile" where ("type" -> kt, "auth_status" -> st)).
                  selectSkipTop (skip)(take)("date")(detailResult(_)).toList)))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryUserWithToken(token : String) : JsValue =
        ((from db() in "user_profile" where ("user_lst.token" -> token)
            select (x => x.getAs[MongoDBList]("user_lst").get)).toList.flatten.filter
                (x => x.asInstanceOf[BasicDBObject].get("token") == token) match {
                  case Nil => ErrorCode.errorToJson("user not exist")
                  case head :: Nil => toJson(userResult(head.asInstanceOf[BasicDBObject]))
                  case _ => ???
        })
        
    def queryInstanceWithToken(token : String) : JsValue = 
        (from db() in "user_profile" where ("user_lst.token" -> token)
            select (x => x)).toList match { 
                  case Nil => ErrorCode.errorToJson("user not exist")
                  case head :: Nil => toJson(detailResult(head))
                  case _ => ???
        }
    
    def queryAdminOpenIdWithToken(token : String) : String = 
        (from db() in "user_profile" where ("user_lst.token" -> token)
            select (x => x)).toList match { 
                  case Nil => ???
                  case head :: Nil => head.getAs[String]("open_id").get
                  case _ => ???
        }
        
    def sendCode(data : JsValue) : JsValue = {
        val phoneNo = (data \ "cell_phone").asOpt[String].map (x => x).getOrElse("")
       
        if (phoneNo.isEmpty) ErrorCode.errorToJson("wrong cell phone")
        else if ((from db() in "user_profile" where (
                    $or("phone_no" -> phoneNo, 
                        "cell_phone" -> phoneNo,
                        "user_lst.indicate" -> phoneNo)) select (x => x)).toList.length > 0) ErrorCode.errorToJson("duplicate phone or email")
        else {
//            val code = scala.util.Random.nextInt(9000) + 1000
            val code = "1111"
            
            (from db() in "reg" where ("cell_phone" -> phoneNo) select (x => x)).toList match {
              case Nil => {
                  val builder = MongoDBObject.newBuilder
                  builder += "cell_phone" -> phoneNo
                  builder += "code" -> code.toString
                  builder += "validate" -> Sercurity.getTimeSpanWith10Minutes
                  
                  _data_connection.getCollection("reg") += builder.result
              }
              case head :: Nil => {
                  head += "code" -> code.toString
                  head += "validate" -> Sercurity.getTimeSpanWith10Minutes
                  
                  _data_connection.getCollection("reg").update(DBObject("cell_phone" -> phoneNo), head)
              }
            }
           
//            import play.api.Play.current
//            smsModule().sendSMS(phoneNo, code.toString)
            toJson(Map("status" -> "ok", "result" -> "send sms message success"))
        }
    }
    
    def updateDriverProfile(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))  
            
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  (data \ "phone_no").asOpt[String].map {x => head += "phone_no" -> x}.getOrElse(Unit) 
                  (data \ "capacity").asOpt[Int].map (x => head += "capacity" -> x.asInstanceOf[Number]).getOrElse(Unit)
                  (data \ "vehicle").asOpt[List[String]].map (x => head += "vehicle" -> x).getOrElse(Unit)
                  (data \ "vehicle_length").asOpt[List[Float]].map (x => head += "vehicle_length" -> x).getOrElse(Unit)
                  (data \ "driver_name").asOpt[String].map (x => head += "driver_name" -> x).getOrElse(Unit)
                  (data \ "driver_social_id").asOpt[String].map (x => head += "driver_social_id" -> x).getOrElse(Unit)
                  (data \ "driver_image").asOpt[String].map (x => head += "driver_image" -> x).getOrElse(Unit)
                  (data \ "road_image").asOpt[String].map (x => head += "road_image" -> x).getOrElse(Unit)
                  
                  _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> "ok", "result" -> "success"))
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def updateProfile(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  (data \ "legal_person").asOpt[String].map (x => head += "legal_person" -> x).getOrElse(Unit)
                  (data \ "legal_person_id").asOpt[String].map (x => head += "legal_person_id" -> x).getOrElse(Unit)
                  (data \ "address").asOpt[String].map (x => head += "address" -> x).getOrElse(Unit)
                  (data \ "phone_no").asOpt[String].map (x => head += "phone_no" -> x).getOrElse(Unit)
                  (data \ "cell_phone").asOpt[String].map (x => head += "cell_phone" -> x).getOrElse(Unit)
                  (data \ "description").asOpt[String].map (x => head += "description" -> x).getOrElse(Unit)
                  
                  (data \ "cell_phone_owner").asOpt[String].map (x => head += "cell_phone_owner" -> x).getOrElse(Unit)
                  (data \ "cell_phone").asOpt[String].map (x => head += "cell_phone" -> x).getOrElse(Unit)
                  
                  (data \ "business_image").asOpt[String].map (x => head += "business_image" -> x).getOrElse(Unit)
                  (data \ "road_image").asOpt[String].map (x => head += "road_image" -> x).getOrElse(Unit)

                  val lines = MongoDBList.newBuilder
                  var bChangeLines = false
                  (data \ "lines").asOpt[List[JsValue]].map { lst => bChangeLines = true; lst foreach { x =>
                      val lb = MongoDBObject.newBuilder
                      lb += "origin_province" -> (x \ "origin_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      lb += "origin_city" -> (x \ "origin_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      lb += "destination_province" -> (x \ "destination_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      lb += "destination_city" -> (x \ "destination_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                      lines += lb.result
                  }}.getOrElse(Unit)
                  
                  val detail = head.getAs[MongoDBObject]("detail").get
                  head.getAs[Number]("type").get.intValue match {
                    case registerTypes.company.t => {
                        if (bChangeLines) detail += "company_lines" -> lines.result
                        else Unit
                        (data \ "web").asOpt[String].map (x => detail += "company_web" -> x).getOrElse(Unit)
                        (data \ "email").asOpt[String].map (x => detail += "company_email" -> x).getOrElse(Unit)
                        (data \ "fax").asOpt[String].map (x => detail += "company_fax" -> x).getOrElse(Unit)
                    }
                    case registerTypes.industry.t => {
                        (data \ "web").asOpt[String].map (x => detail += "industry_web" -> x).getOrElse(Unit)
                        (data \ "email").asOpt[String].map (x => detail += "industry_email" -> x).getOrElse(Unit)
                        (data \ "fax").asOpt[String].map (x => detail += "industry_fax" -> x).getOrElse(Unit) 
                    }
                    case registerTypes.specialway.t => {
                        (data \ "web").asOpt[String].map (x => detail += "special_web" -> x).getOrElse(Unit)
                        (data \ "email").asOpt[String].map (x => detail += "special_email" -> x).getOrElse(Unit)
                        (data \ "fax").asOpt[String].map (x => detail += "special_fax" -> x).getOrElse(Unit)
                        
                        (data \ "special_occation").asOpt[Int].map (x => head += "special_occation" -> x.asInstanceOf[Number]).getOrElse(Unit)
                        (data \ "special_storage").asOpt[List[JsValue]].map { lst => 
                            detail += "special_storage" -> (lst map { x =>
                                val sb = MongoDBObject.newBuilder
                                sb += "province" -> (x \ "province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                                sb += "city" -> (x \ "city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                                sb += "district" -> (x \ "district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                                sb += "address" -> (x \ "address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                                sb.result
                            })
                        }.getOrElse(Unit)
                        (data \ "special_lines").asOpt[List[JsValue]].map { lst =>
                            detail += "special_lines" -> (lst map { x => 
                                val sb = MongoDBObject.newBuilder
                                sb += "origin_province" -> (x \ "origin_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input")) 
                                sb += "origin_city" -> (x \ "origin_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input")) 
                                sb += "destination_province" -> (x \ "destination_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input")) 
                                sb += "destination_city" -> (x \ "destination_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input")) 
                                sb.result
                            })
                        }.getOrElse(Unit)
                        (data \ "vehicle").asOpt[List[String]].map (lst => detail += "vehicle" -> lst).getOrElse(Unit)
                    }
                  }
                  head += "detail" -> detail
                  
                  _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("wrong input")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
   
    def updateAdminPwd(data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val user_id = (data \ "user_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val old = (data \ "old").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pwd = (data \ "pwd").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val user_lst = head.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]
                  val user = user_lst.filter (x => user_id.equals(x.get("user_id"))).head
                  
                  if (old.equals(user.get("pwd"))) user += "pwd" -> pwd
                  else throw new Exception("wrong input")
                  head += "user_lst" -> (user :: (user_lst.filterNot (x => user_id.equals(x.get("user_id")))))
                  
                  _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
              }
              case Nil => throw new Exception("email not exist")
              case _ => throw new Exception("email not exist") 
            }
        
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def updatePwd(data : JsValue) : JsValue = {
//    def updatePwd(open_id : String, user_id : String, data : JsValue) : JsValue = {
        try {
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val token = (data \ "token").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val old = (data \ "old").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val pwd = (data \ "pwd").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            (from db() in "user_profile" where ("open_id" -> open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val user_lst = head.getAs[MongoDBList]("user_lst").get.toList.asInstanceOf[List[BasicDBObject]]
                  val user = user_lst.filter (x => token.equals(x.get("token"))).head
                  
                  if (old.equals(user.get("pwd"))) user += "pwd" -> pwd
                  else throw new Exception("wrong input")
                  head += "user_lst" -> (user :: (user_lst.filterNot (x => token.equals(x.get("token")))))
                  
                  _data_connection.getCollection("user_profile").update(DBObject("open_id" -> open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(this.detailResult(head))))
              }
              case Nil => throw new Exception("email not exist")
              case _ => throw new Exception("email not exist") 
            }
        
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryMultipleProfiles(lst : List[String]) : List[JsValue] = {
        def conditionImpl(l : List[String], cur: Option[DBObject]) : Option[DBObject] = {
            l match {
              case Nil => cur
              case head :: t => cur match {
                                      case None => conditionImpl(t, Some(DBObject("open_id" -> head)))
                                      case Some(x) => conditionImpl(t, Some($or(x, DBObject("open_id" -> head))))
                                  }
            }
        }
       
        conditionImpl(lst, None) match {
          case None => Nil
          case Some(x) => (from db() in "user_profile" where (x) select (detailResult(_))).toList
        }
    }
   
    def authCheck(token : String) : Option[(String, String, Int ,Int)] = {
       
        def authCheckAcc(t : String) : (String, String, Int, Int) = {
            (from db() in ("user_profile") where ("user_lst.token" -> t) select (x => x)).toList match {
              case Nil => ("", "", authStatus.anyStatus.t, authTypes.anyBody.t)
              case head :: Nil => (head.getAs[String]("open_id").get, head.getAs[String]("user_lst.user_id").get, 
                                   head.getAs[Number]("auth_status").map (x => x.intValue).getOrElse(authStatus.progress.t) , 
                                   head.getAs[Number]("user_lst.auth").map (x => x.intValue).getOrElse(authTypes.notAuth.t))
              case _ => null
            }
        }
      
        authCheckAcc(token.substring("Basic ".length())) match {
          case x : (String, String, Int, Int) => Some(x)
          case null => None
        }
    }
}