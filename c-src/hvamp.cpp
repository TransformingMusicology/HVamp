/* HVamp - A Vamp Host in Haskell

Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
Author: Richard Lewis <richard.lewis@gold.ac.uk>

This compilation unit provides a C wrapper over the methods of the
Vamp host SDK.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "hvamp.h"
#include "vamp-hostsdk/PluginLoader.h"
#include "vamp/vamp.h"
#include <vector>
#include <string>
#include <iostream>
#include <string.h>

char ** hvamp_list_plugins() {
    Vamp::HostExt::PluginLoader* loader = Vamp::HostExt::PluginLoader::getInstance();
    Vamp::HostExt::PluginLoader::PluginKeyList list = loader->listPlugins();

    char **keys = new char *[list.size() + 1];
    std::vector<Vamp::HostExt::PluginLoader::PluginKey>::size_type i = 0;
    for (i = 0; i != list.size(); i++) {
        keys[i] = new char[list[i].size() + 1];
        strcpy(keys[i], list[i].c_str());
    }
    keys[i++] = NULL;

    return keys;
}

void hvamp_free_plugin_list(char ** keys) {
  if (keys == NULL) return;
  char **l = keys;
  while (*l != NULL) {
    delete [] *l;
    *l++;
  }
  delete [] keys;
}

const char * hvamp_plugin_key(const char *filepath, const char *identifier) {
    Vamp::HostExt::PluginLoader* loader = Vamp::HostExt::PluginLoader::getInstance();
    return loader->composePluginKey(std::string(filepath), std::string(identifier)).c_str();
}

const char * hvamp_plugin_path(const plugin_key_t key) {
    Vamp::HostExt::PluginLoader* loader = Vamp::HostExt::PluginLoader::getInstance();
    return loader->getLibraryPathForPlugin(Vamp::HostExt::PluginLoader::PluginKey(key)).c_str();
}

const char ** hvamp_plugin_category(const plugin_key_t key) {

}

VampPluginHandle * hvamp_load_plugin(const plugin_key_t key, float input_sample_rate, int adapter_flags, plugin_key_t *plugin) {
    Vamp::HostExt::PluginLoader* loader = Vamp::HostExt::PluginLoader::getInstance();
    Vamp::Plugin* p = loader->loadPlugin(key, input_sample_rate, adapter_flags);
    return (VampPluginHandle*)p;
}

