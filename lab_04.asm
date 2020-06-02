timer_interval equ 10000 / 24 * 100
reverse_base_mantiss_shifted equ 0cccdh
videopage_size equ 80*25
videoline_length equ 80
videocolumn_height equ 25
platform_length equ 8

doodle_attenuation_x equ 1
doodle_max_speed_x equ 4
doodle_initial_speed_x equ 3
doodle_jump_speed equ 10
doodle_fall_speed equ 4
gravity equ 1

points_per_line equ 1
data segment
;   ========================================   40 characters
;   ================================================================================   80 characters
    doodle_x dw 40
    doodle_y dw 24
    doodle_y_delta dw 1

    doodle_speed_x dw 0
    doodle_speed_y dw 10

    doodle_speed_x_saturation dw 0

    current_timer_delay dw 0, 0A2C0h
    ;timer_delays dw 5h, 0910h,   3h, 0D40h,   1, 0ADB0h,   0, 0F424h,   0, 0A2C2h
    ;timer_delays dw 3h, 0D40h,    1, 0ADB0h,    1, 2C7Bh,    0, 0F424h,    0, 0A2C2h
    ;timer_delays dw 2h, 880Ah,    2h, 0E848h,    1h, 0ADB0h,    1h, 2C78h,    0h, 0E5C7h,    0h, 0A2C2h
    ;timer_delays dw 1h, 0E848h,    1h, 0ADB0h,    1h, 631Dh,    1h, 1704h,    0h, 0E5C7h,     0h, 0A2C2h
    ;timer_delays dw 1h, 0ADB0h,    1h, 86A0h,    1h, 4585h,    1h, 1704h,    0h, 0E5C7h,     0h, 0A2C2h
    timer_delays dw 1h, 0ADB0h,    1h, 86A0h,    1h, 4585h,    1h, 1704h,    0h, 0E5C7h,     0h, 0BA03h,    0h, 0A2C2h,    0h, 0A2C2h, 0h, 0A2C2h, 0h, 0A2C2h, 0h, 0A2C2h, 0h, 0A2C2h

    score dw 0
    random_seed dw 0

    ;platform db '=', 00h, '=', 00h, '=', 00h, '=', 00h, '=', 00h, '=', 00h, '=', 00h, '=', 00h
    ;filling db ' ', 00h
    ;platform dw 8 dup(3d00h)
    platform dw 8 dup(073dh)
    empty_lines_counter dw 7
    ;filling dw 0000011100110000b, 0h
    filling dw 0720h

    ;videopages_offsets dd 0b8000000h, 0b8003E80h, 0b8007D00h, 0b800BB80h
    videopages_offsets dd 0b8000000h, 0b8001000h, 0b8002000h, 0b8003000h
    current_videopage dw 0

    warning_timer_error_message db "Timer interruptuion returned error sign. ", 0ah, 0dh, '$'
    info_loop_interrupted_by_user db "Loop was interrupted by keyboard input" , 0ah, 0dh, '$'
    ;<space>:pause/resume;
    ;menubar_info db '<', 07h, 's', 07h, 'p', 07h, 'a', 07h, 'c', 07h, 'e', 07h, '>', 07h, ':', 07h, 'p', 07h, 'a', 07h, 'u', 07h, 's', 07h, 'e', 07h, '/', 07h, 'r', 07h, 'e', 07h, 's', 07h, 'u', 07h, 'm', 07h, 'e', 07h
    ;<exc>:exit;
    menubar_info db '<', 07h, 'e', 07h, 'x', 07h, 'c', 07h, '>', 07h, ':', 07h, 'e', 07h, 'x', 07h, 'i', 07h, 't', 07h

    ;Score: ;
    menubar_score db 'S', 07h, 'c', 07h, 'o', 07h, 'r', 07h, 'e', 07h, ':', 07h, ' ', 07h
    ;<R>:restart;
    menubar_restart db '<', 07h, 'R', 07h, '>', 07h, ':', 07h, 'r', 07h, 'e', 07h, 's', 07h, 't', 07h, 'a', 07h, 'r', 07h, 't', 07h

    ;doodle_map db                             07h, '#', 07h, '#', 07h, '#',
    ;                                07h, '#', 07h, '%', 07h, '#', 07h, '#', 07h, '#',
    ;  07h, '$', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#',
    ;                                07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#',
    ;                                 07h, ']', 07h, ']', 07h, '|', 07h, '[', 07h, '['


     doodle_map db '#', 07h, '#', 07h, '#', 07h, '#', 07h, '%', 07h, '#', 07h, '#', 07h, '#', 07h, '$', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, '#', 07h, ']', 07h, ']', 07h, '|', 07h, '[', 07h, '[', 07h

;         ###       __
;        #%###     /  \
;     $#######  __(%   )
;        ##### *--{====}
;        ]]|[[     """"
;                  ]||[
ends

stack segment
    dw   100h  dup(0)
ends

code segment
assume ds:data, es:data, cs:code, ss:stack

newline macro
    mov ah, 02h
    mov dl, 0ah
    int 21h
    mov dl, 0dh
    int 21h
endm

replace_mul macro a_param, b_param; 80 = 1010000
    shl a_param, 2
    add a_param, b_param
    shl a_param, 4
endm

set_timer_dur macro
    mov dx, timer_interval mod 0ffffh
    mov cx, timer_interval / 0ffffh
endm

set_timer_delay_dynamic macro
    mov dx, [current_timer_delay + 2h]
    mov cx, [current_timer_delay]
endm

generate_platform_position macro; assumed that counter is ont the top of stack
    LOCAL reset_counter, omit_counter_reset
    push cx
    mov cx, empty_lines_counter
    call generate_random
    and ah, 01001010b
    jnz omit_counter_reset
    reset_counter:
    mov cx, 7
    xor ah, ah
    omit_counter_reset:
    dec cx
    jcxz reset_counter
    mov empty_lines_counter, cx
    pop cx
endm

proc output_number
    pushf; save regs
    push ax
    push bx
    push cx
    push dx
    push bp

    mov bp, sp
    mov bx, ss:[bp + 0eh]


    mark4:
    mov ax, bx
    mov cx, reverse_base_mantiss_shifted
    mul cx
    mov cx, 3
    shr dx, cl
    mov cx, dx
    sal dx, 2
    add dx, cx
    sal dx, 1
    mov ax, bx
    sub ax, dx
    push ax

    mov bx, cx
    cmp cx, 0
    jnz mark4

    break:
    mov cx, bp
    sub cx, sp
    sar cx, 1


    mark5:
    pop ax
    add al, '0'
    add ah, 07h
    stosw
    loop mark5
    ;mark5:
    ;pop dx
    ;add dl, '0'
    ;mov ah, 2h
    ;int 21h
    ;loop mark5

    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
output_number endp

proc binary_output
    pushf
    push ax
    push bx
    push cx
    push dx
    push bp

    mov bp, sp
    mov ax, ss:[bp + 0eh]

    mov cx, 0010h

    parse_bitset:
    mov bx, '0'
    rcr ax, 1
    jnc push_binary_digit
    inc bx
    push_binary_digit:
    push bx
    loop parse_bitset

    mov cx, 0010h

    output_parsed_bitset:
    pop dx
    mov ah, 02h
    int 21h
    loop output_parsed_bitset

    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
binary_output endp

proc generate_random
    pushf
    push dx

    mov ax, random_seed
    mov dx, 23973
    mul dx

    add ax, 42359
    mov random_seed, ax

    pop dx
    popf
    ret
generate_random endp

proc scroll_videopage; <- page_num, generate/position (ffxx not generate, 00xx generate, xx - position)
    pushf
    push si
    push di
    push ds
    push es
    push ax
    push cx
    push bp

    mov bp, sp
    mov si, ss:[bp + 12h]
    les di, ds:[si]
    lds si, ds:[si]

    ;add di, (videopage_size - 1) SHL 1
    add di, (videopage_size - videoline_length - 1) SHL 1
    mov si, di
    sub si, videoline_length SHL 1
    std
    ;mov cx, videopage_size - videoline_length
    mov cx, videopage_size - (videoline_length * 2)

    rep movsw

    mov ax, data
    mov ds, ax
    lea si, filling
    mov ax, ds:[si]

    mov cx, videoline_length
    rep stosw

    mov ax, ss:[bp + 14h]
    cmp ah, 0
    jne field_generation_finished
    ;xor ah, ah
    and al, 00111111b; to range 0-63
    cld
    lea si, platform
    mov cx, platform_length
    add ax, 4h
    shl ax, 1
    add di, ax
    rep movsw


    field_generation_finished:
    pop bp
    pop cx
    pop ax
    pop es
    pop ds
    pop di
    pop si
    popf
    ret
scroll_videopage endp

proc copy_videopage; <- to, from
    pushf
    push si
    push di
    push ds
    push es
    push cx
    push bp


    mov bp, sp
    mov si, ss:[bp + 10h]; operand to es:di
    les di, ds:[si]
    mov si, ss:[bp + 12h]; operand to ds:si
    lds si, ds:[si]

    cld
    mov cx, videopage_size
    rep movsw

    pop bp
    pop cx
    pop es
    pop ds
    pop di
    pop si
    popf
    ret
copy_videopage endp

proc move_doodle
    pushf
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp

    mov dx, doodle_speed_y
    mov doodle_y_delta, 1
    cmp dx, 0
    jg positive_y
    jl negative_y
    mov doodle_y_delta, 0
    jmp positive_y
    negative_y:
    neg dx
    mov doodle_y_delta, -1
    cmp dx, doodle_fall_speed
    jle positive_y
    mov dx, doodle_fall_speed
    positive_y:
    lea si, [timer_delays]
    shl dx, 2
    add si, dx
    lea di, [current_timer_delay]
    movsw
    movsw


    sub doodle_speed_y, gravity
    mov ax, doodle_y_delta
    mov bx, doodle_y;-----
    sub bx, ax
    cmp bx, videocolumn_height + 5
    jng continue_game
    call lose_game
    continue_game:
    mov doodle_y, bx
    ;sub doodle_y, ax


    mov ax, doodle_speed_x
    cmp ax, 0
    jg positive_x
    cmp ax, -doodle_max_speed_x + 1
    jg verify_x
    mov ax, -doodle_max_speed_x + 1
    jmp verify_x
    positive_x:
    cmp ax, doodle_max_speed_x - 1
    jl verify_x
    mov ax, doodle_max_speed_x - 1
    verify_x:
    mov doodle_speed_x, ax
    add ax, doodle_x
    cmp ax, videoline_length
    jng reight_bound
    sub ax, videoline_length
    reight_bound:
    cmp ax, 0
    jnl commit_doodle_x_position
    add ax, videoline_length

    ;speed = 1000; => +1 pixel
    ; dur = base_dur / speed_y * 1000
    ; dur = base_dur - rate * speed (speed > 0) -> speed = 0 dur = 1/24

    ;base-dur speed_y = 0 => dur = base_dur
    commit_doodle_x_position:
    mov doodle_x, ax

    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
move_doodle endp

proc print_doodle
    pushf
    push ax
    push bx
    push cx
    push dx
    push es
    push ds
    push si
    push di
    push bp

    mov bp, sp
    mov si, ss:[bp + 16h]
    les di, ds:[si]

    mov ax, doodle_y
    sub ax, 5
    shl ax, 1
    mov dx, ax
    replace_mul ax, dx

    mov bx, doodle_x
    sub bx, 2
    shl bx, 1

    add ax, bx

    lea si, doodle_map
    cld
    add di, ax

    mov cx, 3
    rep movsw

    add di, (videoline_length - 4) SHL 1
    mov cx, 5
    rep movsw

    add di, (videoline_length - 8) SHL 1
    mov cx, 8
    rep movsw

    add di, (videoline_length - 5) SHL 1
    mov cx, 5
    rep movsw

    add di, (videoline_length - 5) SHL 1
    mov cx, 5
    rep movsw

    pop bp
    pop di
    pop si
    pop ds
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
print_doodle endp

proc keyboard_actions
    ;int 16h
    cmp ah, 01h
    je exit_on_esc
    cmp ah, 02h
    je first_page
    cmp ah, 03h
    je second_page
    cmp ah, 04h
    je third_page
    cmp ah, 05h
    je forth_page
    cmp ah, 13h
    je restart_game
    cmp ah, 4dh
    je accelerate_doodle_right
    cmp ah, 4bh
    je accelerate_doodle_left
    ret

    exit_on_esc:
    call exit_program
    ret

    restart_game:
    jmp start
    ret

    first_page:
    mov ax, 0500h
    int 10h
    ret
    second_page:
    mov ax, 0501h
    int 10h
    ret
    third_page:
    mov ax, 0502h
    int 10h
    ret
    forth_page:
    mov ax, 0503h
    int 10h
    ret

    accelerate_doodle_left:
    mov ax, doodle_speed_x
    cmp ax, 0
    jnge further_acceleration_left
    mov ax, -doodle_initial_speed_x
    jmp finish_acceleration_left
    further_acceleration_left:
    sub ax, 2
    ;cmp ax, -doodle_max_speed_x
    ;jnl finish_acceleration_left
    ;mov ax, -doodle_max_speed_x
    finish_acceleration_left:
    mov doodle_speed_x, ax
    ret

    accelerate_doodle_right:
    mov ax, doodle_speed_x
    cmp ax, 0
    jnle further_acceleration_right
    mov ax, doodle_initial_speed_x
    jmp finish_acceleration_right
    further_acceleration_right:
    add ax, 2
    ;cmp ax, doodle_max_speed_x
    ;jng finish_acceleration_right
    ;mov ax, doodle_max_speed_x
    finish_acceleration_right:
    mov doodle_speed_x, ax
    ret
keyboard_actions endp

proc test_jump
    pushf
    push si
    push di
    push ds
    push es
    push cx
    push bp

    mov ax, doodle_speed_y
    cmp ax, 0
    jnl omit_accelerating

    mov bx, platform
    mov bp, sp
    mov si, ss:[bp + 10h]
    les di, ds:[si]

    mov ax, doodle_y
    mov dx, ax
    replace_mul ax, dx
    mov dx, doodle_x
    add ax, dx
    ;add ax, (videoline_length - 2)
    sub ax, 3
    shl ax, 1

    add di, ax
    mov ax, bx
    mov cx, 6
    repne scasw

    ;mov ax, data
    ;mov ds, ax
    ;mov ax, doodle_y
    ;cmp ax, videocolumn_height - 1
    ;jnl accelerate_temp
    jcxz omit_accelerating

    accelerate_temp:
    mov doodle_speed_y, doodle_jump_speed

    omit_accelerating:
    pop bp
    pop cx
    pop es
    pop ds
    pop di
    pop si
    popf
    ret
test_jump endp

proc render_start_map
    mov cx, videocolumn_height - 1
    prerender_mark:
    generate_platform_position
    push ax
    lea dx, [videopages_offsets + 8h]
    push dx
    call scroll_videopage
    add sp, 4h
    loop prerender_mark


    lea dx, [videopages_offsets + 8h]
    push dx
    call print_menubar
    add sp, 2h

    ret
render_start_map endp

proc animate
    mov ax, doodle_speed_y
    cmp ax, -doodle_fall_speed
    jg test_screen_scroll
    mov ax, doodle_y
    cmp ax, videocolumn_height - doodle_jump_speed - 5
    jg omit_map_generation

    test_screen_scroll:
    mov ax, doodle_y
    cmp ax, videocolumn_height - doodle_jump_speed - 2
    jg omit_map_generation

    generate_platform_position; returns at ax platform position/probability
    add score, points_per_line
    push ax; push platform position
    lea dx, [videopages_offsets + 8h]
    push dx
    call scroll_videopage
    call print_menubar
    add sp, 4h

    omit_map_generation:
    lea dx, [videopages_offsets + 8h]
    push dx
    lea dx, [videopages_offsets + 0h]
    push dx
    call copy_videopage
    call print_doodle
    call test_jump
    add sp, 4h
    call move_doodle
    ret
animate endp

proc print_menubar
    pushf
    push si
    push di
    push ds
    push es
    push ax
    push cx
    push bp

    mov bp, sp
    mov si, ss:[bp + 12h]
    les di, ds:[si]

    mov ax, videopage_size - videoline_length
    shl ax, 1
    add di, ax
    lea si, [menubar_info]

    cld
    mov cx, 10
    rep movsw

    add di, (videoline_length - 10 - 13) SHL 1

    mov cx, 7
    lea si, [menubar_score]
    rep movsw

    mov ax, score
    push ax
    call output_number
    add sp, 2h

    pop bp
    pop cx
    pop ax
    pop es
    pop ds
    pop di
    pop si
    popf
    ret
print_menubar endp

proc lose_game
    lea si, [videopages_offsets]
    les di, ds:[si]

    mov ax, videopage_size - videoline_length
    shl ax, 1
    add di, ax
    lea si, [menubar_restart]

    cld
    mov cx, 11
    rep movsw

    mov cx, (videoline_length - 11 - 13); SHL 1
    mov ah, 07h
    mov al, ' '
    rep stosw

    mov cx, 7
    lea si, [menubar_score]
    rep movsw

    mov ax, score
    push ax
    call output_number
    add sp, 2h

    call wait_input

    ret
lose_game endp

proc clear_screen
    pushf
    push ax
    push cx
    push si
    push di
    push es
    lea si, [videopages_offsets]
    les di, ds:[si]
    mov ah, 07h
    mov al, ' '
    mov cx, videopage_size SHL 1
    cld
    rep stosw

    pop es
    pop di
    pop si
    pop cx
    pop ax
    popf
    ret
clear_screen endp

proc exit_program
    call clear_screen

    xor dx, dx
    mov bh, 0h
    mov ah, 02h
    int 10h

    mov ax, 4c00h
    int 21h
    ret
exit_program endp

proc wait_input
    infinite_wait:
    xor al, al
    mov ah, 01h
    int 16h
    je wait_timer
    xor ax, ax
    int 16h
    call keyboard_actions
    wait_timer:
    set_timer_dur; macro, sets timer_interval duration in microseconds
    mov ah, 86h
    int 15h
    jc wait_error
    jmp infinite_wait
    wait_error:
    call exit_program
    ret
wait_input endp

proc initialize_game
    mov doodle_x, 40
    mov doodle_y, 24
    mov doodle_speed_x, 0
    mov doodle_speed_y, 10
    mov doodle_y_delta, 1
    mov score, 0
    ret
initialize_game endp

start:
    mov ax, data
    mov ds, ax
    mov es, ax

    mov ax, 2h
    int 10h
    mov ax, 0500h
    int 10h

    xor ax, ax
    int 1ah
    mov random_seed, ax

    call initialize_game
    call render_start_map


    mov dh, videoline_length
    mov dl, videocolumn_height
    mov bh, 0h
    mov ah, 02h
    int 10h

    mov ah, 01h
    int 21h

    cycle:
    xor al, al
    mov ah, 01h
    int 16h
    je continue
    xor ax, ax
    int 16h
    call keyboard_actions
    continue:
    mov ax, doodle_speed_x
    cmp ax, 0
    jz animation
    jg attenuate_right
    add doodle_speed_x, 1
    jmp animation
    attenuate_right:
    sub doodle_speed_x, 1

    animation:
    call animate

    ;set_timer_dur; macro, sets timer_interval duration in microseconds
    set_timer_delay_dynamic
    mov ah, 86h
    int 15h
    jc error
    jmp cycle

    error:
    newline
    lea dx, warning_timer_error_message
    mov ah, 09h
    int 21h
    call exit_program
ends

end start ; set entry point and stop the assembler.

    ;mov dl, 'a'
    ;mov ah, 02h
    ;int 21h


        ;lea dx, [videopages_offsets + 4h]
        ;push dx
        ;call print_doodle
        ;add sp, 2h

        ;add dx, 4h
        ;push dx
        ;call copy_videopage
        ;add sp, 4h

        ;mov ax, 0501h
        ;int 10h
