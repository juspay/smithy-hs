$version: "2.0"

namespace com.example

use aws.protocols#restJson1

@title("ExampleService")
@restJson1
@httpBearerAuth
service ExampleService {
    version: "2025-30-05"
    operations: [
        GetMenu
        PostMenu
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

/// A structure which defines a coffee item which can be ordered
structure CoffeeItem {
    @required
    @jsonName("coffeeType")
    ctype: CoffeeType

    @required
    description: String
}

union SomeUnion {
    @jsonName("stringType")
    label1: String

    label2: CoffeeType

    label3: CoffeeItem
}

/// A list of coffee items
list CoffeeItems {
    member: CoffeeItem
}

list StringList {
    member: String
}

/// Retrieve the menu
@http(method: "GET", uri: "/menu")
@readonly
operation GetMenu {
    output := {
        items: CoffeeItems
    }
}

// Post the menu
@http(method: "POST", uri: "/menu/{some}?myQuery=123")
operation PostMenu {
    input := {
        item: CoffeeItem

        unionItem: SomeUnion

        // @httpQueryParams
        // stringQueryParams: MapOfString
        @httpQueryParams
        listQueryParams: MapOfListString

        @httpQuery("pageQuery")
        page: Integer

        @httpQuery("type")
        @required
        experimentType: String

        @httpQuery("status")
        @required
        status: StringList

        @httpLabel
        @required
        some: String

        @httpHeader("x-my-header")
        tags: String

        @httpPrefixHeaders("x-useless-")
        versions: MapOfString
    }

    output := {
        @required
        items: CoffeeItem

        @httpPrefixHeaders("x-some-header-")
        resHeaders: MapOfString

        @httpHeader("x-config-tag")
        config_tag: String
    }
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
