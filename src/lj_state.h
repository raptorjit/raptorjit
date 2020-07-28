/*
** State and stack handling.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_STATE_H
#define _LJ_STATE_H

#include <assert.h>
#include "lj_obj.h"

#define incr_top(L) \
  (++L->top >= tvref(L->maxstack) && (lj_state_checkstack(L, 0), 0))

#define savestack(L, p)		((char *)(p) - mref(L->stack, char))
#define restorestack(L, n)	((TValue *)(mref(L->stack, char) + (n)))

static LJ_AINLINE void lj_state_checkstack(lua_State *L, MSize need)
{
  /* XXX This should throw an error rather than abort() */
  assert(mref(L->maxstack, char) - (char *)L->top >=
	 (ptrdiff_t)need*(ptrdiff_t)sizeof(TValue));
}

LJ_FUNC lua_State *lj_state_new(lua_State *L);
LJ_FUNC void lj_state_free(global_State *g, lua_State *L);

#endif
