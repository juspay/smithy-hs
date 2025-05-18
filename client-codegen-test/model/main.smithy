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

        @httpQueryParams
        queryParams: MapOfString

        @httpQuery("pageQuery")
        page: Integer

        @httpLabel
        @required
        some: String
    }

    output := {
        items: CoffeeItems
    }
}

map MapOfString {
    key: String
    value: String
}

// Errors
@httpError(500)
@error("server")
structure InternalServerError {
    message: String
}
