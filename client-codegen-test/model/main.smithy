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
    DRIP
    POUR_OVER
    LATTE
    ESPRESSO
}

/// A structure which defines a coffee item which can be ordered
structure CoffeeItem {
    @required
    ctype: CoffeeType

    @required
    description: String
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
@http(method: "POST", uri: "/menu")
operation PostMenu {
    input := {
        @httpPayload
        item: CoffeeItem
    }

    output := {
        items: CoffeeItems
    }
}

// Errors
@httpError(500)
@error("server")
structure InternalServerError {
    message: String
}
