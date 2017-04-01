# Tankode interface for Ruby
#
# Copyright (C) 2017  Rudy Matela
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1, as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# TODO: make this interface more "classy"
TICKS_PER_SECOND = 120
MAX_GUN_TURN = 2/3r / TICKS_PER_SECOND

module Tankode
  def self.run
    STDIN.flush
    STDOUT.flush
    STDIN.sync = true
    STDOUT.sync = true
    STDIN.each_line do |line|
      i, s, e, w = line.split.map{|s| s == "-" ? nil : s.to_r}
      return if i == 0
      a, b, g, r, s = yield i, s, e, w
      puts "#{toIncDec a} #{toIncDec b} #{g} #{r} #{s}"
    end
  end

  def self.toIncDec(r)
    if r > 0
      '+'
    elsif r < 0
      '-'
    else
      '='
    end
  end
end
