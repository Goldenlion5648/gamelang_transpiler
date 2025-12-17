package output

import "core:fmt"
import rl "vendor:raylib"

// AUTO GENERATED,
// CHANGES WILL BE OVERRIDDEN
cat: i64
testing: i64
screen_width: i64
screen_height: i64
hi: string
exact: f64
global_test: i64



init :: proc() {
// Globals init
cat = 5
testing = 124
screen_width = 1280
screen_height = 720
hi = "hello"
exact = 4.234
global_test = 123
using rl
rl.SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
rl.InitWindow(screen_width, screen_height, "Odin + Raylib on the web")
    for i in 1 ..< 10  {
        fmt.println("count")


    }
}


update :: proc() {
    if testing > 4 {
        fmt.println("greater")
    }
    else {
        fmt.println("less")


    }
}

make_grid :: proc( x_dim : int ) -> int {
    return 6

}

main :: proc() {
    init()
    update()
}

