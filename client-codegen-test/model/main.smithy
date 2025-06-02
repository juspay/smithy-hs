$version: "2.0"

namespace com.example

use aws.protocols#restJson1

@title("ExampleService")
@restJson1
@httpBearerAuth
service ExampleService {
    version: "2025-30-05"
    operations: [
        TestHttpLabels
        TestQuery
        TestHttpHeaders
        // TestHttpPayloadSer
        // TestHttpDocumentSer
        // TestHttpPayloadOutputSer
        // TestHttpDocumenetOutputSer
        // TestAllFeaturesSer
    ]
    errors: [
        InternalServerError
    ]
}

/// An enum describing the types of coffees available
enum CoffeeType {
    @enumValue("Drip")
    DRIP

    POUR_OVER

    LATTE

    ESPRESSO
}

/// Types of milk available for coffee
enum MilkType {
    WHOLE
    SKIM
    OAT
    ALMOND
    SOY
    NONE
}

/// Temperature preferences
enum TemperaturePreference {
    HOT
    ICED
    EXTRA_HOT
}

/// A structure which defines a coffee item which can be ordered
structure CoffeeItem {
    @required
    @jsonName("type")
    coffeeType: CoffeeType

    @required
    description: String
}

/// Represents different types of coffee customizations
union CoffeeCustomization {
    milk: MilkType
    temperature: TemperaturePreference
}

/// A list of coffee items
list CoffeeItems {
    member: CoffeeItem
}

list StringList {
    member: String
}

map MapOfString {
    key: String
    value: String
}

map MapOfListString {
    key: String
    value: StringList
}

// Errors
@httpError(500)
@error("server")
structure InternalServerError {
    message: String
}

@http(method: "GET", uri: "/path_params/{identifier}/{enabled}/{name}")
@readonly
operation TestHttpLabels {
    input := {
        @httpLabel
        @required
        identifier: Integer

        @httpLabel
        @required
        enabled: Boolean

        @httpLabel
        @required
        name: String
    }

    output := {
        @required
        message: String
    }
}

@http(method: "GET", uri: "/query_params?query_literal=some_query_literal_value")
@readonly
operation TestQuery {
    input := {
        @httpQuery("page")
        page: Integer

        @httpQuery("type")
        coffeeType: String

        @httpQuery("enabled")
        enabled: Boolean

        @httpQuery("tags")
        tags: StringList

        @httpQueryParams
        mapQueryParams: MapOfString
    }

    output := {
        @required
        message: String
    }
}

// test httpHeader which can be of type int, string, bool, of a list of these types
@http(method: "GET", uri: "/headers")
@readonly
operation TestHttpHeaders {
    input := {
        @httpHeader("x-header-int")
        intHeader: Integer

        @httpHeader("x-header-string")
        stringHeader: String

        @httpHeader("x-header-bool")
        boolHeader: Boolean

        @httpHeader("x-header-list")
        listHeader: StringList

        @httpPrefixHeaders("x-prefix-")
        prefixHeaders: MapOfString
    }

    output := {
        @required
        message: String
    }
}
// // Test for httpPayload, also including query params and http labels and some headers
// @http(method: "POST", uri: "/payload/{identifier}")
// operation TestHttpPayloadSer {
//     input := {
//         @httpPayload
//         @required
//         payload: CoffeeItem
//         @httpLabel
//         @required
//         identifier: Integer
//         @httpHeader("x-header-string")
//         stringHeader: String
//         @httpPrefixHeaders("x-prefix-")
//         prefixHeaders: MapOfString
//     }
//     output := {
//         @required
//         message: String
//     }
// }
// // Test for httpPayload, also including query params and http labels and some headers
// @http(method: "POST", uri: "/document/{identifier}")
// operation TestHttpDocumentSer {
//     input := {
//         payload: CoffeeItem
//         customization: CoffeeCustomization
//         @httpLabel
//         @required
//         identifier: Integer
//         @httpHeader("x-header-string")
//         stringHeader: String
//         @httpPrefixHeaders("x-prefix-")
//         prefixHeaders: MapOfString
//     }
//     output := {
//         @required
//         message: String
//     }
// }
// // Test for httpPayload and headers in the output
// @http(method: "GET", uri: "/payload_output")
// @readonly
// operation TestHttpPayloadOutputSer {
//     input := {
//         @httpQuery("type")
//         coffeeType: String
//     }
//     output := {
//         @httpHeader("x-output-header")
//         outputHeader: String
//         @httpHeader("x-output-header-int")
//         outputHeaderInt: Integer
//         @httpHeader("x-output-header-bool")
//         outputHeaderBool: Boolean
//         @httpHeader("x-output-header-list")
//         outputHeaderList: StringList
//         @httpPrefixHeaders("x-output-prefix-")
//         outputPrefixHeaders: MapOfString
//         @httpPayload
//         item: CoffeeItem
//     }
// }
// // Test for httpPayload and headers in the output
// @http(method: "GET", uri: "/document_output")
// @readonly
// operation TestHttpDocumenetOutputSer {
//     input := {
//         @httpQuery("type")
//         coffeeType: String
//     }
//     output := {
//         @httpHeader("x-output-header")
//         outputHeader: String
//         @httpHeader("x-output-header-int")
//         outputHeaderInt: Integer
//         @httpHeader("x-output-header-bool")
//         outputHeaderBool: Boolean
//         @httpHeader("x-output-header-list")
//         outputHeaderList: StringList
//         @httpPrefixHeaders("x-output-prefix-")
//         outputPrefixHeaders: MapOfString
//         item: CoffeeItem
//         customization: CoffeeCustomization
//     }
// }
// // Test operation which has all of above
// @http(method: "POST", uri: "/all_features/{identifier}")
// operation TestAllFeaturesSer {
//     input := {
//         @httpPayload
//         @required
//         payload: CoffeeItem
//         @httpLabel
//         @required
//         identifier: Integer
//         @httpQuery("page")
//         page: Integer
//         @httpQuery("type")
//         coffeeType: CoffeeType
//         @httpQuery("enabled")
//         enabled: Boolean
//         @httpQuery("tags")
//         tags: StringList
//         @httpQueryParams
//         mapQueryParams: MapOfString
//         @httpHeader("x-header-int")
//         intHeader: Integer
//         @httpHeader("x-header-string")
//         stringHeader: String
//         @httpHeader("x-header-bool")
//         boolHeader: Boolean
//         @httpHeader("x-header-list")
//         listHeader: StringList
//         @httpPrefixHeaders("x-prefix-")
//         prefixHeaders: MapOfString
//     }
//     output := {
//         // all outputs
//         @httpHeader("x-output-header")
//         outputHeader: String
//         @httpHeader("x-output-header-int")
//         outputHeaderInt: Integer
//         @httpHeader("x-output-header-bool")
//         outputHeaderBool: Boolean
//         @httpHeader("x-output-header-list")
//         outputHeaderList: StringList
//         @httpPrefixHeaders("x-output-prefix-")
//         outputPrefixHeaders: MapOfString
//         item: CoffeeItem
//         customization: CoffeeCustomization
//     }
// }
