#!/bin/bash

#  This file is part of Quickwin.
#
#  Quickwin is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Quickwin is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Quickwin.  If not, see <https://www.gnu.org/licenses/>.

cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1

lua quickwin-main.lua
