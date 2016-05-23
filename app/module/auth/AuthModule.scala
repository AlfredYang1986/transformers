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

object authTypes {
    case object admin extends authTypeDefines(0, "admin")
    case object driverMaster extends authTypeDefines(1, "driver master")
    case object companyMaster extends authTypeDefines(2, "company master")
    case object companyOthers extends authTypeDefines(3, "company others")
}

sealed abstract class authTypeDefines(val t : Int, val des : String)

object registerTypes {
    case object driver extends registerTypesDefines(0, "driver")
    case object company extends registerTypesDefines(1, "company")
    case object industry extends registerTypesDefines(2, "industry")
    case object spicalway extends registerTypesDefines(3, "spicalway")
}

sealed abstract class registerTypesDefines(val t : Int, val des : String)

object businessTypes {
    case object car extends businessTypesDefines(0, "公路")
    case object train extends businessTypesDefines(1, "铁路")
    case object ship extends businessTypesDefines(2, "船运")
    case object plane extends businessTypesDefines(3, "航空")
}

sealed abstract class businessTypesDefines(val t : Int, val des : String)

object AuthModule {
    def register(data : JsValue) : JsValue = {
     
        def commonRegisterImpl(x : MongoDBObject) : (Boolean, String) =
            try {
                (data \ "company_name").asOpt[String].map (tmp => x += "company_name" -> tmp).getOrElse(throw new Exception("input company name"))
                (data \ "legal_person").asOpt[String].map (tmp => x += "legal_person" -> tmp).getOrElse(throw new Exception("input legal person"))
                (data \ "legal_person_id").asOpt[String].map (tmp => x += "legal_person_id" -> tmp).getOrElse(throw new Exception("input legal person id"))
                (data \ "address").asOpt[String].map (tmp => x += "address" -> tmp).getOrElse(throw new Exception("input company reg address"))
                (data \ "phone_dir").asOpt[String].map (tmp => x += "phone_dir" -> tmp).getOrElse(x += "phone_dir" -> "")
                (data \ "phone_no").asOpt[String].map (tmp => x += "phone_no" -> tmp).getOrElse(x += "phone_no" -> "")
                (data \ "phone_sep").asOpt[String].map (tmp => x += "phone_sep" -> tmp).getOrElse(x += "phone_sep" -> "")
                
                (true, "")
            } catch {
              case ex : Exception => (false, ex.getMessage)
            }
      
        def companyRegisterImpl(x : MongoDBObject) : (Boolean, String) = {
            try {
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
            (false, "")
        }
        
        def spicalwayRegisterImpl(x : MongoDBObject) : (Boolean, String) = {
            (false, "")
        }
        
        def createBasicAccount(x : MongoDBObject) = {
            val company_name = (data \ "company_name").asOpt[String].get
            val company_email = (data \ "company_email").asOpt[String].get
            val company_type = (data \ "company_type").asOpt[Int].get

            val user_id = Sercurity.md5Hash(company_name + company_email + Sercurity.getTimeSpanWithMillSeconds)
            x += "user_id" -> user_id
            x += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
            x += "type" -> company_type.asInstanceOf[Number]
            x += "auth" -> authTypes.companyMaster.t.asInstanceOf[Number]
        }
        
        val company_type = (data \ "company_type").asOpt[Int].map (x => x).getOrElse(throw new Exception("Bad Input"))
       
        val common = MongoDBObject.newBuilder.result
        val (common_result, common_error) = commonRegisterImpl(common)
        if (!common_result) ErrorCode.errorToJson(common_error)
        else {
            import registerTypes._
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
               createBasicAccount(company_master)
               user_lst += company_master
               common += "user_lst" -> user_lst.result
               
               _data_connection.getCollection("companies") += common
               toJson(Map("status" -> "ok", "result" -> "register success"))
            }
        }
    }
    
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