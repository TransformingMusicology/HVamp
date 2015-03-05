/* HVamp - A Vamp Host for Haskell

Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
Author: Richard Lewis <richard.lewis@gold.ac.uk>

This compilation unit provides a C wrapper over the Vamp plugin
discovery and loading methods of the Vamp host SDK.

This file is part of HVamp

HVamp is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

HVamp is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with HVamp. If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef _HVAMP_VAMP_SDK_H_
#define _HVAMP_VAMP_SDK_H_

#include "vamp/vamp.h"

#ifdef __cplusplus
extern "C"
{
#endif //__cplusplus

typedef char* plugin_key_t;

typedef struct vamp_plugin {} vamp_plugin_t;

const char ** hvamp_list_plugins();
const char * hvamp_plugin_key(const char *filepath, const char *identifier);
const char * hvamp_plugin_path(const plugin_key_t key);
const char ** hvamp_plugin_category(const plugin_key_t key);
VampPluginHandle * hvamp_load_plugin(const plugin_key_t key, float input_sample_rate, int adapter_flags);

#ifdef __cplusplus
}
#endif //__cplusplus
#endif //_HVAMP_VAMP_SDK_H_
