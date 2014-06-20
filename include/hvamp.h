/* HVamp - A Vamp Host in Haskell

Copyright (C) 2014 Richard Lewis, Goldsmiths' College
Author: Richard Lewis <richard.lewis@gold.ac.uk>

This compilation unit provides a C wrapper over the methods of the
Vamp host SDK.

*/

#ifndef _HVAMP_VAMP_SDK_H_
#define _HVAMP_VAMP_SDK_H_

#ifndef __cplusplus
extern "C"
#endif //__cplusplus

typedef *char plugin_key_t;

typedef struct vamp_plugin {} vamp_plugin_t;

int hvamp_list_plugins(char **plugin_key_t);
char * hvamp_plugin_key(const char *filepath, const char *identifier);
char * hvamp_plugin_path(const char *key);
char * hvamp_plugin_category(const char *key);
int hvamp_load_plugin(const char *key, float input_sample_rate, int adapter_flags, plugin_key_t *plugin);

#endif //_HVAMP_VAMP_SDK_H_
