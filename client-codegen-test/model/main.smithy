$version: "2.0"

namespace com.example

use aws.protocols#restJson1

@title("ExampleService")
@restJson1
@httpBearerAuth
@httpBasicAuth
@auth([httpBearerAuth, httpBasicAuth])
service ExampleService {
    version: "2025-30-05"
    operations: [
        TestHttpLabels
        TestQuery
        TestHttpHeaders
        TestHttpPayload
        TestHttpDocument
        TestHttpPayloadDeserialization
        TestHttpDocumentDeserialization
        TestReservedWords
        TestCustomStatus
        TestErrors
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

    @timestampFormat("http-date")
    @required
    createdAt: Timestamp

    @timestampFormat("date-time")
    utc: Timestamp

    posix: Timestamp
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

@http(method: "GET", uri: "/path_params/{identifier}/{enabled}/{name}/{time}/{utc}/{posix}")
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

        @httpLabel
        @required
        @timestampFormat("http-date")
        time: Timestamp

        @httpLabel
        @required
        @timestampFormat("date-time")
        utc: Timestamp

        @httpLabel
        @required
        posix: Timestamp
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

        @httpQuery("utc")
        @timestampFormat("date-time")
        utc: Timestamp

        @httpQuery("posix-ts")
        posixTs: Timestamp

        @httpQuery("time")
        @timestampFormat("http-date")
        time: Timestamp

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

        @httpHeader("x-header-time")
        @timestampFormat("http-date")
        time: Timestamp

        @httpHeader("x-header-utc")
        @timestampFormat("date-time")
        utc: Timestamp

        @httpHeader("x-header-posix")
        posix: Timestamp

        @httpPrefixHeaders("x-prefix-")
        prefixHeaders: MapOfString
    }

    output := {
        @required
        message: String
    }
}

@http(method: "POST", uri: "/payload/{identifier}")
operation TestHttpPayload {
    input := {
        @httpPayload
        @required
        payload: CoffeeItem

        @httpLabel
        @required
        identifier: Integer

        @httpHeader("x-header-string")
        stringHeader: String

        @httpPrefixHeaders("x-prefix-")
        prefixHeaders: MapOfString
    }

    output := {
        @required
        message: String
    }
}

@http(method: "POST", uri: "/document/{identifier}")
operation TestHttpDocument {
    input := {
        payload: CoffeeItem

        customization: CoffeeCustomization

        @timestampFormat("http-date")
        time: Timestamp

        @httpLabel
        @required
        identifier: Integer

        @httpHeader("x-header-string")
        stringHeader: String

        @httpPrefixHeaders("x-prefix-")
        prefixHeaders: MapOfString
    }

    output := {
        @required
        message: String
    }
}

@http(method: "GET", uri: "/payload_response")
@readonly
operation TestHttpPayloadDeserialization {
    input := {
        @httpQuery("type")
        coffeeType: String
    }

    output := {
        @httpHeader("x-output-header")
        outputHeader: String

        @httpHeader("x-output-header-int")
        outputHeaderInt: Integer

        @httpHeader("x-output-header-bool")
        outputHeaderBool: Boolean

        @httpHeader("x-output-header-time")
        @timestampFormat("http-date")
        time: Timestamp

        @httpHeader("x-output-header-utc")
        @timestampFormat("date-time")
        utcHeader: Timestamp

        @httpHeader("x-output-header-posix")
        posixHeader: Timestamp

        @httpHeader("x-output-header-list")
        outputHeaderList: StringList

        @httpPrefixHeaders("x-output-prefix-")
        outputPrefixHeaders: MapOfString

        @httpPayload
        item: CoffeeItem
    }
}

@http(method: "GET", uri: "/document_response")
@readonly
operation TestHttpDocumentDeserialization {
    input := {
        @httpQuery("type")
        coffeeType: String
    }

    output := {
        @httpHeader("x-output-header")
        outputHeader: String

        @httpHeader("x-output-header-int")
        outputHeaderInt: Integer

        @httpHeader("x-output-header-bool")
        outputHeaderBool: Boolean

        @httpHeader("x-output-header-list")
        outputHeaderList: StringList

        @httpPrefixHeaders("x-output-prefix-")
        outputPrefixHeaders: MapOfString

        item: CoffeeItem

        customization: CoffeeCustomization

        @timestampFormat("http-date")
        time: Timestamp
    }
}

@http(method: "POST", uri: "/reserved-words")
operation TestReservedWords {
    input := {
        @required
        type: String

        @required
        data: String

        @required
        as: String

        @required
        case: String

        @required
        class: String

        @required
        default: String

        @required
        deriving: String

        @required
        do: String

        @required
        else: String

        @required
        hiding: String

        @required
        if: String

        @required
        import: String

        @required
        in: String

        @required
        infix: String

        @required
        infixl: String

        @required
        infixr: String

        @required
        instance: String

        @required
        let: String

        @required
        module: String

        @required
        newtype: String

        @required
        of: String

        @required
        qualified: String

        @required
        then: String

        @required
        where: String
    }

    output := {
        @required
        type: String

        @required
        data: String

        @required
        as: String

        @required
        case: String

        @required
        class: String

        @required
        default: String

        @required
        deriving: String

        @required
        do: String

        @required
        else: String

        @required
        hiding: String

        @required
        if: String

        @required
        import: String

        @required
        in: String

        @required
        infix: String

        @required
        infixl: String

        @required
        infixr: String

        @required
        instance: String

        @required
        let: String

        @required
        module: String

        @required
        newtype: String

        @required
        of: String

        @required
        qualified: String

        @required
        then: String

        @required
        where: String
    }
}

@http(method: "POST", uri: "/custom-status", code: 201)
operation TestCustomStatus {
    input := {}

    output := {
        @required
        message: String
    }
}

@error("client")
@httpError(400)
structure Error400 {
    @required
    message: String
}

@http(method: "POST", uri: "/error-4xx")
operation TestErrors {
    input := {}
    output := {}
    errors: [
        Error400
    ]
}
