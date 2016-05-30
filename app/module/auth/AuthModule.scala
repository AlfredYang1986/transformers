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

object authStatus {  // auth  indicate weather account is approved 
    case object approved extends authTypeDefines(0, "approved")
    case object progress extends authTypeDefines(1, "in progress")
    case object rejected extends authTypeDefines(2, "rejected")
}

sealed abstract class authStatusDefines(val t : Int, val des : String)

object authTypes {  // auth  indicate which account 
    case object admin extends authTypeDefines(0, "admin")
    case object driverMaster extends authTypeDefines(1, "driver master")
    case object companyMaster extends authTypeDefines(2, "company master")
    case object companyOthers extends authTypeDefines(3, "company others")
}

sealed abstract class authTypeDefines(val t : Int, val des : String)

object registerTypes {  // type indicate type of account
    case object driver extends registerTypesDefines(0, "driver")
    case object company extends registerTypesDefines(1, "company")
    case object industry extends registerTypesDefines(2, "industry")
    case object spicalway extends registerTypesDefines(3, "spicalway")
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
              case spicalway.t => "special_email"
            })).asOpt[String].get

            val user_id = Sercurity.md5Hash(name + email + Sercurity.getTimeSpanWithMillSeconds)
            x += "user_id" -> user_id
            x += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
            x += "auth" -> authTypes.companyMaster.t.asInstanceOf[Number]
        }
       
        val common = MongoDBObject.newBuilder.result
        val (common_result, common_error) = commonRegisterImpl(common)
        if (!common_result) ErrorCode.errorToJson(common_error)
        else {
            val detail = MongoDBObject.newBuilder.result
            val (detail_result, detail_error) = company_type match {
              case company.t => companyRegisterImpl(detail)
              case industry.t => industryRegisterImpl(detail)
              case spicalway.t => spicalwayRegisterImpl(detail)
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
               
               _data_connection.getCollection("companies") += common
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
            (data \ "driver_image").asOpt[String].map (tmp => x += "business_image" -> tmp).getOrElse(throw new Exception("input drive image"))
            (data \ "road_image").asOpt[String].map (tmp => x += "road_image" -> tmp).getOrElse(throw new Exception("input drive road image"))
            x += "auth_status" -> authStatus.progress.t.asInstanceOf[Number]
            x += "open_id" -> Sercurity.md5Hash(driver_secial_id + Sercurity.getTimeSpanWithMillSeconds)
            x += "type" -> registerTypes.driver.t.asInstanceOf[Number]
           
            val user_lst = MongoDBList.newBuilder
            val company_master = MongoDBObject.newBuilder.result
            createBasicAccount(company_master)
            user_lst += company_master
            x += "user_lst" -> user_lst.result
            
            _data_connection.getCollection("drivers") += x.result
    
            toJson(Map("status" -> toJson("ok"), "result" -> userResult(company_master)))
        } catch {
            case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
   
    def userResult(x : MongoDBObject) : JsValue =
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "token" -> toJson(x.getAs[String]("token").get)))
    
    def detailResult(x : MongoDBObject) : JsValue = 
        toJson(Map("open_id" -> toJson(x.getAs[String]("open_id").get), "details" -> toJson(
                   x.getAs[Number]("type").get.intValue match {
                      case registerTypes.driver.t => Map(
                          "dirver_name" -> toJson(x.getAs[String]("driver_name").get),
                          "dirver_secial_id" -> toJson(x.getAs[String]("driver_secial_id").get),
                          "capacity" -> toJson(x.getAs[Number]("capacity").get.intValue),
                          "vehicle_length" -> toJson(x.getAs[Number]("vehicle_length").get.intValue),
                          "insurance" -> toJson(x.getAs[Number]("insurance").get.intValue),
                          "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                          "driver_image" -> toJson(x.getAs[String]("driver_image").get),
                          "road_image" -> toJson(x.getAs[String]("road_image").get),
                          "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                          "vehicle" -> toJson(x.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                          "driver_lines" -> toJson(x.getAs[MongoDBList]("driver_lines").get.toList.asInstanceOf[List[MongoDBObject]].map (tmp => 
                              toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                                         "origin_city" -> tmp.getAs[String]("origin_city").get,
                                         "destination_province" -> tmp.getAs[String]("destination_province").get,
                                         "destination_city" -> tmp.getAs[String]("destination_city").get)))))
                      case registerTypes.company.t => Map(
                          "company_name" -> toJson(x.getAs[String]("company_name").get),
                          "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                          "legal_secial_id" -> toJson(x.getAs[String]("legal_secial_id").get),
                          "address" -> toJson(x.getAs[String]("address").get),
                          "bussiness_image" -> toJson(x.getAs[String]("business_image").get),
                          "road_image" -> toJson(x.getAs[String]("road_image").get),
                          "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                          "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                          "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                          "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                          
                          "company_business" -> toJson(x.getAs[Number]("company_business").get.intValue match {
                            case businessTypes.car.t => businessTypes.car.des
                            case businessTypes.plane.t => businessTypes.plane.des
                            case businessTypes.ship.t => businessTypes.ship.des
                            case businessTypes.train.t => businessTypes.train.des
                          }),
                          "company_web" -> toJson(x.getAs[String]("company_web").get),
                          "company_fax" -> toJson(x.getAs[String]("company_fax").get),
                          "company_email" -> toJson(x.getAs[String]("company_email").get),
                          "company_lines" -> toJson(x.getAs[MongoDBList]("company_lines").get.toList.asInstanceOf[List[MongoDBObject]].map (tmp => 
                              toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                                         "origin_city" -> tmp.getAs[String]("origin_city").get,
                                         "destination_province" -> tmp.getAs[String]("destination_province").get,
                                         "destination_city" -> tmp.getAs[String]("destination_city").get)))))
                      case registerTypes.industry.t => Map(
                          "company_name" -> toJson(x.getAs[String]("company_name").get),
                          "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                          "legal_secial_id" -> toJson(x.getAs[String]("legal_secial_id").get),
                          "address" -> toJson(x.getAs[String]("address").get),
                          "bussiness_image" -> toJson(x.getAs[String]("business_image").get),
                          "road_image" -> toJson(x.getAs[String]("road_image").get),
                          "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                          "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                          "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                          "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                          
                          "industry_web" -> toJson(x.getAs[String]("industry_web").get),
                          "industry_fax" -> toJson(x.getAs[String]("industry_fax").get),
                          "industry_email" -> toJson(x.getAs[String]("industry_email").get)
                      )
                      case registerTypes.spicalway.t => Map(
                          "company_name" -> toJson(x.getAs[String]("company_name").get),
                          "legal_person" -> toJson(x.getAs[String]("legal_person").get),
                          "legal_secial_id" -> toJson(x.getAs[String]("legal_secial_id").get),
                          "address" -> toJson(x.getAs[String]("address").get),
                          "bussiness_image" -> toJson(x.getAs[String]("business_image").get),
                          "road_image" -> toJson(x.getAs[String]("road_image").get),
                          "auth_status" -> toJson(x.getAs[Number]("auth_status").get.intValue),
                          "phone_dir" -> toJson(x.getAs[String]("phone_dir").get),
                          "phone_no" -> toJson(x.getAs[String]("phone_no").get),
                          "phone_sep" -> toJson(x.getAs[String]("phone_sep").get),
                          
                          "vehicle" -> toJson(x.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                          "special_web" -> toJson(x.getAs[String]("special_web").get),
                          "special_fax" -> toJson(x.getAs[String]("special_fax").get),
                          "special_email" -> toJson(x.getAs[String]("special_email").get),
                          "special_storage" -> toJson(x.getAs[MongoDBList]("special_storage").get.toList.asInstanceOf[List[MongoDBObject]].map (tmp => 
                              toJson(Map("province" -> tmp.getAs[String]("province").get,
                                         "city" -> tmp.getAs[String]("city").get,
                                         "address" -> tmp.getAs[String]("address").get)))),
                          "special_lines" -> toJson(x.getAs[MongoDBList]("special_lines").get.toList.asInstanceOf[List[MongoDBObject]].map (tmp => 
                              toJson(Map("origin_province" -> tmp.getAs[String]("origin_province").get,
                                         "origin_city" -> tmp.getAs[String]("origin_city").get,
                                         "destination_province" -> tmp.getAs[String]("destination_province").get,
                                         "destination_city" -> tmp.getAs[String]("destination_city").get)))))
                      case _ => ???
                   })))
    
    def login(data : JsValue) : JsValue = {
        null
    }
    
    def admainLogin(data : JsValue) : JsValue = {
        null
    }
   
    def DB2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "token" -> toJson(x.getAs[String]("token").get),
                   "email" -> toJson(x.getAs[String]("email").get),
                   "name" -> toJson(x.getAs[String]("name").get),
                   "type" -> toJson(x.getAs[Number]("id_type").get.intValue),
                   "register_id"-> toJson(x.getAs[String]("register_id").get),
                   "status"-> toJson(x.getAs[Number]("status").get.intValue),
                   "approved_date"-> toJson(x.getAs[Number]("approved_date").get.longValue)))
    
    def queryProfile(user_id : String, data : JsValue) : JsValue = {
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "users" where ("user_id" -> user_id) select (DB2JsValue(_))).toList.head)))
    }
    
    def queryProfileWithToken(token : String) : JsValue = {
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "users" where ("token" -> token) select (DB2JsValue(_))).toList.head)))
    }
    
    def updateProfile(user_id : String, data : JsValue) : JsValue = {
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

              toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
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
                                      case None => conditionImpl(t, Some(DBObject("user_id" -> head)))
                                      case Some(x) => conditionImpl(t, Some($or(x, DBObject("user_id" -> head))))
                                  }
            }
        }
       
        conditionImpl(lst, None) match {
          case None => Nil
          case Some(x) => (from db() in "users" where (x) select (DB2JsValue(_))).toList
        }
    }
   
    def adminAuthCheck(user_id : String) : Boolean = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x.getAs[String]("email").get)).toList match {
          case Nil => false
          case head :: Nil => head.equals("admin")
          case _ => false
        }
    }
    
    def authCheck(token : String) : Option[(String, Int)] = {
       
        def authCheckAcc(t : String) : (String, Int) = {
            (from db() in ("users") where ("token" -> t) select (x => x)).toList match {
              case Nil => null
//              case head :: Nil => (head.getAs[String]("user_id").get, head.getAs[Number]("status").map (x => x.intValue).getOrElse(RegisterApprovedStatus.notApproved.s))
              case _ => null
            }
        }
      
        authCheckAcc(token.substring("Basic ".length())) match {
          case x : (String, Int) => Some(x)
          case null => None
        }
    }
    
    def authCheckUser(user_id : String)(x : JsValue)(func : (String, JsValue) => JsValue) : JsValue = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
          case Nil => ErrorCode.errorToJson("email not exist")
          case head :: Nil => func(user_id, x)
          case _ => ErrorCode.errorToJson("email already reg")
        }
    }
}