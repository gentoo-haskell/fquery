#include "opts.h"

static bool colour = true;

bool colour_enabled(void)
{
    return colour;
}

void set_colour_enabled(const bool on)
{
    colour = on;
}
