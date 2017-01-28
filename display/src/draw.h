#ifndef _DRAW_H
#define _DRAW_H

#include "tankode.h"
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>

void drawCircle(float cx, float cy, float r);
void glColor(struct colour);

void drawField(struct field);
void drawObstacle(struct obstacle);
void drawBullet(struct bullet);
void drawTank(struct tank);
void drawBase(struct tank);
void drawTurret(struct tank);
void drawRadar(struct tank);
void drawScan(struct tank);

#endif /* _DRAW_H */
