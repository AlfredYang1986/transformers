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
    
    case object adminBase extends authTypeDefines(100, "admin base")
    case object admin extends authTypeDefines(101, "admin")
    case object adminMaster extends authTypeDefines(102, "admin master")
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

object insuranceStatus {  // driver insurance indicate of driver insurance
    case object insuranced extends insuranceStatusDefines(0, "已购买")
    case object not_insuranced extends insuranceStatusDefines(1, "未购买")
}

sealed abstract class insuranceStatusDefines(val t : Int, val des : String)

object AuthModule {
    def register(data : JsValue) : JsValue = {
        val company_type = (data \ "company_type").asOpt[Int].map (x => x).getOrElse(throw new Exception("Bad Input"))

        def commonRegisterImpl(x : MongoDBObject) : (Boolean, String) =
            try {
                val company_name = (data \ "company_name").asOpt[String].map (tmp => tmp).getOrElse(throw new Exception("input company name"))
                x += "company_name" -> company_name
//                (data \ "company_name").asOpt[String].map (tmp => x += "company_name" -> tmp).getOrElse(throw new Exception("input company name"))
                (data \ "legal_person").asOpt[String].map (tmp => x += "legal_person" -> tmp).getOrElse(throw new Exception("input legal person"))
                (data \ "legal_person_id").asOpt[String].map (tmp => x += "legal_person_id" -> tmp).getOrElse(throw new Exception("input legal person id"))
                (data \ "address").asOpt[String].map (tmp => x += "address" -> tmp).getOrElse(throw new Exception("input company reg address"))
                (data \ "phone_dir").asOpt[String].map (tmp => x += "phone_dir" -> tmp).getOrElse(x += "phone_dir" -> "")
                (data \ "phone_no").asOpt[String].map (tmp => x += "phone_no" -> tmp).getOrElse(x += "phone_no" -> "")
                (data \ "phone_sep").asOpt[String].map (tmp => x += "phone_sep" -> tmp).getOrElse(x += "phone_sep" -> "")
                (data \ "business_image").asOpt[String].map (tmp => x += "business_image" -> tmp).getOrElse(throw new Exception("input business image"))
                (data \ "road_image").asOpt[String].map (tmp => x += "road_image" -> tmp).getOrElse(throw new Exception("input road image"))
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
              
                (data \ "company_business").asOpt[Int].map (tmp => x += "company_business" -> tmp.asInstanceOf[Number]).getOrElse(throw new Exception("input company business"))
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
            })).asOpt[String].get

            val user_id = Sercurity.md5Hash(name + email + Sercurity.getTimeSpanWithMillSeconds)
            x += "user_id" -> user_id
            x += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
            x += "auth" -> authTypes.companyMaster.t.asInstanceOf[Number]
            x += "indicate" -> email
            x += "pwd" -> "Passw0rd"
            x += "screen_name" -> "company master"
        }
       
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
            (data \ "vehicle_length").asOpt[Int].map (tmp => x += "vehicle_length" -> tmp.asInstanceOf[Number]).getOrElse(x += "vehicle_length" -> 0)
            (data \ "insurance").asOpt[Int].map (tmp => x += "insurance" -> tmp.asInstanceOf[Number]).getOrElse(x += "insurance" -> not_insuranced.t)
            (data \ "capacity").asOpt[Int].map (tmp => x += "capacity" -> tmp.asInstanceOf[Number]).getOrElse(x += "capacity" -> 0)
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
                "capacity" -> toJson(x.getAs[Number]("capacity").get.intValue),
                "vehicle_length" -> toJson(x.getAs[Number]("vehicle_length").get.intValue),
                "insurance" -> toJson(x.getAs[Number]("insurance").get.intValue),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "driver_image" -> toJson(x.getAs[String]("driver_image").get),
                "road_image" -> toJson(x.getAs[String]("road_image").get),
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
                "road_image" -> toJson(x.getAs[String]("road_image").get),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                
                "company_business" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[Number]("company_business").get.intValue match {
                  case businessTypes.car.t => businessTypes.car.des
                  case businessTypes.plane.t => businessTypes.plane.des
                  case businessTypes.ship.t => businessTypes.ship.des
                  case businessTypes.train.t => businessTypes.train.des
                }),
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
                "road_image" -> toJson(x.getAs[String]("road_image").get),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                
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
                "road_image" -> toJson(x.getAs[String]("road_image").get),
                "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                
                "vehicle" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                "special_web" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_web").get),
                "special_fax" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_fax").get),
                "special_email" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[String]("special_email").get),
                "special_storage" -> toJson(x.getAs[MongoDBObject]("detail").get.getAs[MongoDBList]("special_storage").get.toList.asInstanceOf[List[BasicDBObject]].map (tmp => 
                    toJson(Map("province" -> tmp.getAs[String]("province").get,
                               "city" -> tmp.getAs[String]("city").get,
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
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                ((from db() in "user_profile" where ("user_lst" $elemMatch($and("indicate" $eq indicate, "pwd" $eq pwd))) 
                    select (x => x.getAs[MongoDBList]("user_lst").get)).toList.flatten.filter
                        (x => x.asInstanceOf[BasicDBObject].get("indicate") == indicate && x.asInstanceOf[BasicDBObject].get("pwd") == pwd)) match {
                  case Nil => ErrorCode.errorToJson("user not exist")
                  case head :: Nil => toJson(userResult(head.asInstanceOf[BasicDBObject]))
                  case _ => ???
            })))
        }
    }
    
//    def adminLogin(open_id : String, user_id : String, data : JsValue) : JsValue = {
    def adminLogin(data : JsValue) : JsValue = {
        val result = login(data)
        if ((result \ "result" \ "auth").asOpt[Int].map (x => x).getOrElse(authTypes.anyBody.t) > authTypes.adminBase.t) result
        else ErrorCode.errorToJson("user not exist")
    }
    
    def queryProfile(open_id : String, user_id : String, data : JsValue) : JsValue = {
        val query_open_id = (data \ "query_open_id").asOpt[String].map (x => x).getOrElse("")
     
        if (query_open_id == "") ErrorCode.errorToJson("error input")
        else {
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                (from db() in "user_profile" where ("open_id" -> query_open_id) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("error input")
                  case head :: Nil => detailResult(head)
                  case _ => ???
                })))
        }
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
    
    def updateProfile(open_id : String, user_id : String, data : JsValue) : JsValue = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
          case head :: Nil => {
              (data \ "pwd").asOpt[String].map { x => 
                 head += "pwd" -> x 
                 val email = head.getAs[String]("email").get
                 head += "token" -> Sercurity.md5Hash(email + x) 
              }.getOrElse(Unit)
//              (data \ "name").asOpt[String].map (x => head += "name" -> x).getOrElse(Unit)
//              (data \ "register_id").asOpt[String].map (x => head += "register_id" -> x).getOrElse(Unit)
//              (data \ "id_type").asOpt[Int].map (x => head += "id_type" -> x.asInstanceOf[Number]).getOrElse(Unit)
//              (data \ "status").asOpt[Int].map (x => head += "status" -> x.asInstanceOf[Number]).getOrElse(Unit)
//              (data \ "approved_date").asOpt[Long].map (x => head += "approved_date" -> x.asInstanceOf[Number]).getOrElse(Unit)
              
              _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)

              toJson(Map("status" -> toJson("ok"), "result" -> toJson(this.detailResult(head))))
          }
          case Nil => ErrorCode.errorToJson("email not exist") 
          case _ => ErrorCode.errorToJson("email not exist") 
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