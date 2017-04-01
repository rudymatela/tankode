/* draw.h -- drawing of entities for the Tankode game using OpenGL
 *
 * Copyright (C) 2017  Rudy Matela
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef _DRAW_H
#define _DRAW_H

#include "tankode.h"
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <stdio.h>

enum layer {
	floor_layer,
	floor_line_layer,
	track_layer,
	explosion_layer,
	obstacle_layer,
	base_layer,
	gun_layer,
	scan_layer,
	dot_layer,
	radar_layer,
	bullet_layer,
	integrity_layer,
	n_layers
};

void drawCircle(float cx, float cy, float r, enum layer l);
void drawDrop(float cx, float cy, float r, float th, enum layer l);
void drawLine(float x0, float y0, float x1, float y1, float thickness, enum layer l);
void drawRectangle(float x0, float y0, float x1, float y1, float thickness, enum layer l);
void glColor(struct colour);
void glColorAlpha(struct colour, float alpha);
void glDumpPixels(FILE *f);
void screenshot();

void drawField(struct field);
void drawObstacle(struct obstacle);
void drawBullet(struct bullet);
void drawTank(struct tank, struct colour obstacle_colour);
void drawBase(struct tank);
void drawTurret(struct tank);
void drawRadar(struct tank);
void drawScan(struct tank);
void drawIntegrityBar(struct tank);
void drawPowerBar(struct tank);
void drawShotFlare(struct tank);
void drawExplosions(struct tank);

#endif /* _DRAW_H */
