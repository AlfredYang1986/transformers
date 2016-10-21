package module.common.xml

import scala.xml._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue


object xmlOpt {
    lazy val doc_cities = xml.XML.loadFile("resource/city_defines.xml")
    
    var allCities_val : List[JsValue] = Nil
                       
    def allCities : List[JsValue] = 
      if (allCities_val.isEmpty) {
        allCities_val = 
            ((doc_cities \ "province") map ( x => 
                toJson(Map("province" -> toJson((x \ "@name").text),
                           "cities" -> toJson((x \ "city") map (y => y \ "@name") map (y => y.text)),
                           "city_ditails" -> toJson((x \ "city") map ( c => 
                               Map("city" -> toJson((c \ "@name").text),
                                   "districts" -> toJson((c \ "district") map (d => (d \ "@name")) map (z => (z.text))))
                           )))))).toList
        allCities_val
      } else allCities_val
}
