package output

import "core:fmt"
import rl "vendor:raylib"

// AUTO GENERATED,
// CHANGES WILL BE OVERRIDDEN
cat: i32
testing: i32
screen_width: i32
screen_height: i32
game_clock: i32
should_run: bool
hi: string
exact: f32
global_test: i32


init :: proc() {
// init setup
cat = 5
testing = 124
screen_width = 1280
screen_height = 720
game_clock = 0
should_run = true
hi = "hello"
exact = .25
global_test = 123
using rl
SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
InitWindow(screen_width, screen_height, "Odin + Raylib on the web")
SetTargetFPS(60)

// end init setup

    for i in 1 ..< 10  {
        fmt.println("count")


    }
}


update :: proc() {
using rl
BeginDrawing()
ClearBackground(SKYBLUE)

    // if testing > 4:
    //     print("greater")
    // else:
    //     print("less")
    // DrawRectangle(20, 80, 200, 20, RED)
    if IsKeyPressed ( .C ) {
        should_run=false



    }

EndDrawing()
free_all(context.temp_allocator)
}

make_grid :: proc( x_dim : int ) -> int {
    return 6



}

main :: proc() {
    using rl
    init()
    for should_run && ! WindowShouldClose ( )  {
        update()
    }
    CloseWindow()

}

