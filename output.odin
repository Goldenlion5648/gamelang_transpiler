game_clock = 0
should_run = true
max_collectables = 20
score = 0

// var window_name: cstring = "score more"


Collectable :: struct {
    x, y:i32,
    size:i32,

}

init :: proc() {
// init setup
game_clock = 0
should_run = true
max_collectables = 20
score = 0
using rl
SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
InitWindow(screen_width, screen_height, "score more")
SetTargetFPS(60)

// end init setup

    

}

update :: proc() {
using rl
BeginDrawing()
ClearBackground(SKYBLUE)

    if IsKeyPressed ( .C ) {
        should_run=false

    }
    if game_clock % 60 == 0 {
        score+=1

    }
    if len ( collectables ) < max_collectables {
        append(&collectables, Collectable{GetRandomValue(5, screen_width-5), GetRandomValue(5, screen_height-5), GetRandomValue(5, 50), })

    }
    for collectable in collectables  {
        DrawRectangle(collectable.x, collectable.y, collectable.size*5, collectable.size*5, RED)

    }
        DrawText(fmt.ctprintf("%d", score), 8, 8, 40, BLACK)
    


EndDrawing()
game_clock += 1
free_all(context.temp_allocator)
}

main :: proc() {
    using rl
    init()
    for should_run && ! WindowShouldClose ( )  {
        update()
    }
    CloseWindow()
}

