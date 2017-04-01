/* draw.c -- drawing of entities for the Tankode game using OpenGL
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
 *
 *
 * (development started by Rudy Matela on 2016-12-22 18:30)
 */
#include <math.h>
#include "draw.h"
#include <stdio.h>

/* don't import the following, and process the state and sounds in a separate
 * file */
#include "tankode-audio.h"

/* always make the following divisible by 2 and 3 */
#define CIRCLE_SEGMENTS 30
#define MAX_SCREEN_N_PIXELS (4096*2160*8) /* for screenshot and glDumpPixels */

extern int draw_charge;
extern int draw_health;
extern int draw_scan;

static void rotate(float *x, float *y, float dir);

float lay(enum layer l)
{
	return (float)l / (float)n_layers;
}

/* rotate given x and y destructively */
static void rotate(float *x, float *y, float dir)
{
	float x0 = *x,
	      y0 = *y;
	*x = x0 * cos(dir) - y0 * sin(dir);
	*y = x0 * sin(dir) + y0 * cos(dir);
}


void mix(struct colour *c, const struct colour *d, float amount)
{
	c->r = c->r * (1 - amount) + d->r * amount;
	c->g = c->g * (1 - amount) + d->g * amount;
	c->b = c->b * (1 - amount) + d->b * amount;
	c->a = c->a * (1 - amount) + d->a * amount;
}


void glColor(struct colour c)
{
	glColor4f(c.r, c.g, c.b, c.a);
}


void glColorAlpha(struct colour c, float a)
{
	glColor4f(c.r, c.g, c.b, a);
}


void drawCircle(float cx, float cy, float r, enum layer l)
{
	int i;
	float angle, x, y, z = lay(l);
	glBegin(GL_TRIANGLE_FAN);
	glVertex3f(cx,cy,z);
	for (i = 0; i <= CIRCLE_SEGMENTS; i++) {
		angle = 2.0 * M_PI * (float)i / (float)CIRCLE_SEGMENTS;
		x = r * cosf(angle);
		y = r * sinf(angle);
		glVertex3f(x + cx, y + cy, z);
	}
	glEnd();
}


void drawDrop(float cx, float cy, float r, float th, enum layer l)
{
	float z = lay(bullet_layer);
	float x = cos(M_PI/6.) * r;
	float y = sin(M_PI/6.) * r;
	float lx = - x, ly = - y;
	float rx = + x, ry = - y;
	float bx =   0, by = - 2*r;
	drawCircle(cx, cy, r, l);
	rotate(&lx, &ly, th);
	rotate(&rx, &ry, th);
	rotate(&bx, &by, th);
	glBegin(GL_TRIANGLES);
	glVertex3f(cx + lx,cy + ly, z);
	glVertex3f(cx + rx,cy + ry, z);
	glVertex3f(cx + bx,cy + by, z);
	glEnd();
}


/* TODO: fix drawSemiCircle, it is not working well; then, use on drawPill */
void drawSemiCircle(float cx, float cy, float r, float angle0)
{
	int i;
	float angle, x, y;
	glBegin(GL_TRIANGLE_FAN);
	glVertex2f(cx,cy);
	for (i = 0; i <= CIRCLE_SEGMENTS / 2; i++) {
		angle = angle0 + 2.0 * M_PI * (float)i / (float)CIRCLE_SEGMENTS;
		x = r * cosf(angle);
		y = r * sinf(angle);
		glVertex2f(x + cx, y + cy);
	}
	glEnd();
}


void drawPill(float x0, float y0, float x1, float y1, float thickness, enum layer l)
{
	drawRectangle(x0, y0, x1, y1, thickness, l);
	drawCircle(x0, y0, thickness / 2, l);
	drawCircle(x1, y1, thickness / 2, l);
}


void drawLine(float x0, float y0, float x1, float y1, float thickness, enum layer l)
{
	drawRectangle(x0,y0,x1,y1,thickness, l);
}


void drawRectangle(float x0, float y0, float x1, float y1, float thickness, enum layer l)
{
	float
		opp = y1 - y0,
		adj = x1 - x0,
		hyp = sqrt(opp*opp + adj*adj),
		sin = opp / hyp,
		cos = adj / hyp;
	float
		x0l = x0 + sin * thickness / 2.,
		y0l = y0 - cos * thickness / 2.,
		x0r = x0 - sin * thickness / 2.,
		y0r = y0 + cos * thickness / 2.,
		x1l = x1 + sin * thickness / 2.,
		y1l = y1 - cos * thickness / 2.,
		x1r = x1 - sin * thickness / 2.,
		y1r = y1 + cos * thickness / 2.;
	float z = lay(l);
    glBegin(GL_QUADS);
	glVertex3f(x0l, y0l, z);
	glVertex3f(x0r, y0r, z);
	glVertex3f(x1r, y1r, z);
	glVertex3f(x1l, y1l, z);
	glEnd();
}


void drawBullet(struct bullet b)
{
	drawDrop(b.x, b.y, (1 + b.charge + sqrt(b.charge)) / 30., b.dir, bullet_layer);
}

void drawExplosion(struct tank t, struct explosion b)
{
	float minr = (1 + b.charge + sqrt(b.charge)) / 30.,
	      maxr = minr * 4,
		  r = (maxr-minr) * ((float)b.age+1)/EXPLOSION_DISCARD_AGE + minr;
	glColorAlpha(t.bullet_colour, 1. - ((float)b.age)/EXPLOSION_DISCARD_AGE);
	if (b.age == 0)
		play_wall_explosion(b.x, b.y, b.charge);
	drawCircle(b.x, b.y, r, explosion_layer);
}

void drawExplosions(struct tank t)
{
	int i;
	for (i=0; i<t.n_explosions; i++)
		drawExplosion(t, t.explosions[i]);
}


void drawTank(struct tank t, struct colour obstacle)
{
	int i;
	float fade = 7./12.;
	if (t.integrity <= 0) {
		mix(&t.base_colour  ,&obstacle,fade);
		mix(&t.turret_colour,&obstacle,fade);
		mix(&t.radar_colour ,&obstacle,fade);
		mix(&t.track_colour ,&obstacle,fade);
		if (t.previous_integrity > 0)
			play_tank_disable(t.x,t.y);
	}
	drawBase(t);
	drawTurret(t);
	drawRadar(t);
	if (t.integrity > 0) {
		if (draw_health) drawIntegrityBar(t);
		if (draw_charge) drawPowerBar(t);
	}
	glColor(t.bullet_colour);
	drawShotFlare(t);
	glColor(t.bullet_colour);
	for (i=0; i<t.n_bullets; i++)
		drawBullet(t.bullets[i]);
	if (t.integrity > 0 && draw_scan)
		drawScan(t);
}


void drawIntegrityBar(struct tank t)
{
	const float length = 5./6.;
	float y  =   9./12.,
	      x = t.integrity * length / 2.;
	glColor4f(1.,1.,1.,2./3.);
	drawPill(t.x-x,t.y+y,t.x+x,t.y+y,2./30.,integrity_layer);
}


void drawPowerBar(struct tank t)
{
	const float length = 5./6.;
	float x  = 9./12.,
	      y  = t.power * length / 2.;
	glColor4f(1.,1.,1.,2./3.);
	drawPill(t.x+x,t.y-y,t.x+x,t.y+y,2./30.,integrity_layer);
}


void drawRadar(struct tank t)
{
	float z = lay(radar_layer);
	float flx=-1./6., fly=+1./24., frx=+1./6., fry=+1./24.,
	      bx=.0, by=-1./12.;
	glColor(t.radar_colour);
	rotate(&flx, &fly, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&frx, &fry, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&bx,  &by,  t.radar_dir + t.turret_dir + t.base_dir);
	glBegin(GL_TRIANGLES);
	glVertex3f(t.x + flx, t.y + fly, z);
	glVertex3f(t.x + frx, t.y + fry, z);
	glVertex3f(t.x + bx,  t.y + by,  z);
	glEnd();
}


void drawScan(struct tank t)
{
	float fx=0, fy=t.scan_dist+0.5, bx=0, by=0.5;
	rotate(&fx, &fy, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&bx, &by, t.radar_dir + t.turret_dir + t.base_dir);
	t.scan_colour.a = 1./3.; glColor(t.scan_colour);
	drawLine(t.x + bx, t.y + by, t.x + fx, t.y + fy, 1./30., scan_layer);
	t.scan_colour.a = 1./1.; glColor(t.scan_colour);
	drawCircle(t.x + fx, t.y + fy, 1./30., dot_layer);
}


void drawTurret(struct tank t)
{
	float z = lay(gun_layer);
	float flx=-3./48., fly=+7./12., frx=+3./48., fry=+7./12.,
	      blx=-2./24., bly=+.0,     brx=+2./24., bry=+.0;
	glColor(t.turret_colour);
	drawCircle(t.x, t.y, 13./48., gun_layer);
	rotate(&flx,&fly,t.turret_dir+t.base_dir);
	rotate(&frx,&fry,t.turret_dir+t.base_dir);
	rotate(&blx,&bly,t.turret_dir+t.base_dir);
	rotate(&brx,&bry,t.turret_dir+t.base_dir);
    glBegin(GL_QUADS);
    glVertex3f(t.x + flx, t.y + fly, z);
    glVertex3f(t.x + frx, t.y + fry, z);
    glVertex3f(t.x + brx, t.y + bry, z);
    glVertex3f(t.x + blx, t.y + bly, z);
    glEnd();
}


void drawShotFlare(struct tank t)
{
	static const struct colour white = {1., 1., 1., 1.};
	mix(&t.bullet_colour, &white, .66);
	glColorAlpha(t.bullet_colour, .66);
	if (t.heat >= 1.)
		play_shot(t.flare_x, t.flare_y, t.bullets[t.n_bullets-1].charge);
	if (t.heat >= 1./2.)
		drawDrop(t.flare_x, t.flare_y, t.heat / 6., t.flare_dir+M_PI, bullet_layer);
}


void drawBase(struct tank t)
{
	float flx=- 21./60., fly=+24./60., frx=+21./60., fry=+24./60.,
	      blx=- 21./60., bly=-24./60., brx=+21./60., bry=-24./60.;

	rotate(&flx, &fly, t.base_dir); rotate(&frx, &fry, t.base_dir);
	rotate(&blx, &bly, t.base_dir); rotate(&brx, &bry, t.base_dir);

	glColor(t.track_colour);
	drawPill(t.x + flx, t.y + fly, t.x + blx, t.y + bly, 14./60., track_layer);
	drawPill(t.x + frx, t.y + fry, t.x + brx, t.y + bry, 14./60., track_layer);

	glColor(t.base_colour);
	drawCircle(t.x, t.y, 1./2., base_layer);
}


void drawField(struct field field)
{
	int i;
	float f;
	float z = lay(floor_layer);
	glColor(field.colour);
	glBegin(GL_QUADS);
	glVertex3f(0.,          0.,           z);
	glVertex3f(0.,          field.height, z);
	glVertex3f(field.width, field.height, z);
	glVertex3f(field.width, 0.          , z);
	glEnd();
	glColor4f(0.,0.,0.,1./2.);
	// field.obstacle_colour.a = 1./6.; glColor(field.obstacle_colour);
	for(f=0.; f <= field.height; f+=1.) {
		drawLine(0.,f,field.width,f,3./60.,floor_line_layer);
	}
	for(f=0.; f <= field.width; f+=1.) {
		drawLine(f,0.,f,field.height,3./60.,floor_line_layer);
	}
	field.obstacle_colour.a = 1.;
	glColor(field.obstacle_colour);
	for (i=0; i<field.n_obstacles; i++)
		drawObstacle(field.obstacles[i]);
}


void drawObstacle(struct obstacle obstacle)
{
	int i;
	float z = lay(obstacle_layer);
	glBegin(GL_TRIANGLE_FAN);
	for (i=0; i<obstacle.n; i++)
		glVertex3f(obstacle.x[i], obstacle.y[i], z);
	glEnd();
}


/* WARNING: this function fails when called > 1 000 000 000 times */
void screenshot()
{
	static int i = 0;
	char fname[0x100]; /* FIXME: magic number :-D */
	FILE *out;
	sprintf(fname,"%04i.pnm",i++);
	fprintf(stderr,"%s\n",fname);
	out = fopen(fname, "w");
	if (!out) {
		fprintf(stderr,"could not open file\n");
		exit(1);
	}
	glDumpPixels(out);
	fclose(out);
}


void glDumpPixels(FILE *f)
{
	int w=glutGet(GLUT_WINDOW_WIDTH),
	    h=glutGet(GLUT_WINDOW_HEIGHT);
	size_t sz = 3*w*h;
	unsigned char *data = malloc(MAX_SCREEN_N_PIXELS);
	int i,j,k;
	if (!data)
		fprintf(stderr,"error: out of memory\n");
	if (sz > MAX_SCREEN_N_PIXELS) {
		fprintf(stderr,"error: screen size greater than limit.\n"
		               "Hello from the past (2017),\n"
		               "you are using a screen larger than 4k.\n"
		               "What a glorious future to live in, I envy you.\n"
		               "See source code for details.\n");
		exit(1);
		/* Between the calls to glutGet and glReadPixels
		 * the screen size might have changed and was causing a runtime error
		 * (I am using a tiling window manager).
		 * So, I set the size of data to a *very high* default.
		 * HINT: you can try using glReadnPixels,
		 * it was causing link error at the time of this writing.
		 */
	}
	glReadPixels(0, 0, w, h, GL_RGB, GL_UNSIGNED_BYTE, data);
	fprintf(f,"P3\n");
	fprintf(f,"%d %d\n",w,h);
	fprintf(f,"255\n");
	for (i=h-1; i>=0; i--) for (j=0; j<w; j++) {
		k = (i*w+j)*3;
		fprintf(f,"%u %u %u%c",
			data[k],
			data[k+1],
			data[k+2],
			j==w-1?'\n':' ');
	}
}
