#include "bstack.c"
#include "bstackhoriz.c"

static const char font[]            = "-*-terminus-*-r-*-*-12-*-*-*-*-*-iso10646-*";
static const char normbordercolor[] = "#202020";
static const char normbgcolor[]     = "#202020";
static const char normfgcolor[]     = "#a0a0a0";
static const char selbordercolor[]  = "#ffc03a";
static const char selbgcolor[]      = "#404040";
static const char selfgcolor[]      = "#ffc03a";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */
static const int nmaster            = 1;        /* number of clients in master area */
static char dmenumon[2]             = "0";      /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[]       = { "dmenu_run", \
                                        "-m", dmenumon, \
                                        "-fn", font,\
                                        "-nb", normbgcolor, \
                                        "-nf", normfgcolor, \
                                        "-sb", selbgcolor, \
                                        "-sf", selfgcolor, NULL };

static const char *tags[] = { "inet", "work", "media", "irc", "tmp1", "tmp2" };

static const Rule rules[] = {
        /* class      instance    title       tags mask     isfloating   monitor */
        { "Gimp",     NULL,       NULL,       0,            True,        -1 },
        { "Firefox",  NULL,       NULL,       1 << 8,       False,       -1 },
	{ "Pidgin",   NULL,       NULL,       0,            True,        -1 },
};

static const float mfact      = 0.60;  /* factor of master area size [0.05..0.95] */
static const Bool resizehints = False; /* True means respect size hints in tiled resizals */

static const Layout layouts[] = {
        /* symbol     arrange function */
        { "[MONOCLE]",      monocle     },
        { "[BSTACK]",       bstack      },
        { "[BSTACKHORIZ]",  bstackhoriz },
        { "[TILE]",         tile        },
        { "[FLOATING]",     NULL        },  /* no layout function means floating behavior */
};

#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
        { MODKEY,                       KEY, view,       {.ui = 1 << TAG} }, \
        { MODKEY|ControlMask,           KEY, toggleview, {.ui = 1 << TAG} }, \
        { MODKEY|ShiftMask,             KEY, tag,        {.ui = 1 << TAG} }, \
        { MODKEY|ControlMask|ShiftMask, KEY, toggletag,  {.ui = 1 << TAG} },

#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

//static const char *cmd_iceweasel[] = { "firefox",                           NULL };
static const char *cmd_urxvt[]     = { "urxvtc",                            NULL };
static const char *cmd_wlan_con[]  = { "wlan", "-c",                        NULL };
static const char *cmd_wlan_dis[]  = { "wlan", "-d",                        NULL };
static const char *cmd_xlock[]     = { "xlock-wrapper",                     NULL };
//static const char *cmd_xterm[]     = { "urxvtc", "-name", "urxvt2",         NULL };
static const char *snd_repeat[]    = { "cmus-remote", "-s", "-p",           NULL };
static const char *snd_pause[]     = { "cmus-remote", "-u",                 NULL };
static const char *snd_vol_mute[]  = { "amixer", "set", "Master", "toggle", NULL };
static const char *snd_vol_dec[]   = { "amixer", "set", "PCM", "5%-",       NULL };
static const char *snd_vol_inc[]   = { "amixer", "set", "PCM", "5%+",       NULL };
/* static const char *dmenucmd[]   = { "dmenu_run", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL }; */

static Key keys[] = {
        /* modifier           key         function         argument */
        //{ MODKEY,             XK_a,       setlayout,       {0} }, // use prev layouts
        { MODKEY,             XK_a,       setlayout,       { .v = &layouts[0]} },  // bstack
        { MODKEY,             XK_s,       setlayout,       { .v = &layouts[1]} },
        { MODKEY,             XK_7,       setlayout,       { .v = &layouts[2]} },
        { MODKEY,             XK_8,       setlayout,       { .v = &layouts[4]} },

        { MODKEY,             XK_b,       spawn,           {.v = snd_vol_dec } },
        { MODKEY,             XK_v,       togglebar,       {0} },
        { MODKEY|ShiftMask,   XK_c,       quit,            {0} },
	{ MODKEY,             XK_c,       spawn,           {.v = snd_pause } },
        //{ MODKEY,             XK_d,       spawn,           {.v = cmd_xterm } },
        { MODKEY,             XK_d,       spawn,           SHCMD("urxvtc -name urxvt2") },
        //{ MODKEY,             XK_e,       spawn,           {.v = cmd_iceweasel } },
        { MODKEY,             XK_e,       spawn,           SHCMD("/data/firefox/firefox/firefox") },
        { MODKEY,             XK_f,       spawn,           {.v = cmd_urxvt } },
        { MODKEY,             XK_g,       spawn,           SHCMD("killall -q conky || conky -q") },
        { MODKEY,             XK_i,       incnmaster,      {.i = +1 } },
        { MODKEY,             XK_d,       incnmaster,      {.i = -1 } },
        { MODKEY,             XK_h,       setmfact,        {.f = -0.05} },
        { MODKEY,             XK_i,       spawn,           SHCMD("echo 4 > /proc/acpi/ibm/cmos") },
        { MODKEY,             XK_j,       focusstack,      {.i = +1 } },
        { MODKEY,             XK_k,       focusstack,      {.i = -1 } },
        { MODKEY,             XK_l,       setmfact,        {.f = +0.05} },
        { MODKEY,             XK_m,       spawn,           {.v = snd_vol_mute } },
	{ MODKEY,             XK_n,       spawn,           {.v = snd_vol_inc } },
        { MODKEY,             XK_o,       spawn,           {.v = cmd_xlock } },
        { MODKEY,             XK_q,       killclient,      {0} },
        { MODKEY,             XK_t,       spawn,           {.v = cmd_wlan_con } },
        { MODKEY,             XK_y,       spawn,           {.v = cmd_wlan_dis } },
        { MODKEY,             XK_p,       spawn,           SHCMD("echo 5 > /proc/acpi/ibm/cmos") },
        { MODKEY,             XK_x,       spawn,           {.v = snd_repeat } },
        { MODKEY,             XK_Return,  zoom,            {0} },
        { MODKEY,             XK_Tab,     view,            {0} },
        { MODKEY|ShiftMask,   XK_space,   togglefloating,  {0} },
        { MODKEY,             XK_0,       view,            {.ui = ~0 } },
        { MODKEY|ShiftMask,   XK_0,       tag,             {.ui = ~0 } },
        { MODKEY,             XK_comma,   focusmon,        {.i = -1 } },
        { MODKEY,             XK_period,  focusmon,        {.i = +1 } },
        { MODKEY|ShiftMask,   XK_comma,   tagmon,          {.i = -1 } },
        { MODKEY|ShiftMask,   XK_period,  tagmon,          {.i = +1 } },
        TAGKEYS(              XK_1,                        0)
        TAGKEYS(              XK_2,                        1)
        TAGKEYS(              XK_3,                        2)
        TAGKEYS(              XK_4,                        3)
        TAGKEYS(              XK_5,                        4)
        TAGKEYS(              XK_6,                        5)
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
        /* click                event mask      button          function        argument */
        { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
        { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
        { ClkWinTitle,          0,              Button2,        zoom,           {0} },
        { ClkStatusText,        0,              Button2,        spawn,          {.v = cmd_urxvt } },
        { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
        { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
        { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
        { ClkTagBar,            0,              Button1,        view,           {0} },
        { ClkTagBar,            0,              Button3,        toggleview,     {0} },
        { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
        { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
