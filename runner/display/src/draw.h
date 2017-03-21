#ifndef _DRAW_H
#define _DRAW_H

#include "tankode.h"
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>

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
