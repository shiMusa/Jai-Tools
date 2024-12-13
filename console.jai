#import "Basic";


ConsoleColor :: enum {
    FG_RESET;
    FG_BLACK;
    FG_RED;
    FG_GREEN;
    FG_YELLOW;
    FG_BLUE;
    FG_MAGENTA;
    FG_CYAN;
    FG_WHITE;
}

ansi_color :: (c: ConsoleColor = .FG_RESET) -> string {
    if c == {
        case .FG_RESET  ; return "\e[0m";
        case .FG_BLACK  ; return "\e[30m";
        case .FG_RED    ; return "\e[31m";
        case .FG_GREEN  ; return "\e[32m";
        case .FG_YELLOW ; return "\e[33m";
        case .FG_BLUE   ; return "\e[34m";
        case .FG_MAGENTA; return "\e[35m";
        case .FG_CYAN   ; return "\e[36m";
        case .FG_WHITE  ; return "\e[37m";
    }
    return ansi_color();
}

ansi_rgb_color :: (r: u8, g: u8, b: u8, $$foreground: bool = true) -> string {
    if foreground then return sprint("\e[38;2;%;%;%m", r, g, b);
    else               return sprint("\e[48;2;%;%;%m", r, g, b);
}


color_text :: inline (txt: string, color: string) -> string {
    return sprint("%1%2%3", color, txt, ansi_color());
}


fg_color :: inline (txt: string, r: u8, g: u8, b: u8) -> string {
    return sprint("%1%2%3", ansi_rgb_color(r,g,b), txt, ansi_color());
}

bg_color :: inline (txt: string, r: u8, g: u8, b: u8) -> string {
    return sprint("%1%2%3", ansi_rgb_color(r,g,b, false), txt, ansi_color());
}