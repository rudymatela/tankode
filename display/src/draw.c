#include <math.h>
#include "draw.h"
#include <stdio.h>

#define CIRCLE_SEGMENTS 30

/* rotate given x and y destructively */
void rotate(float *x, float *y, float dir)
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


void drawCircle(float cx, float cy, float r)
{
	int i;
	float angle, x, y;
	glBegin(GL_TRIANGLE_FAN);
	glVertex2f(cx,cy);
	for (i = 0; i <= CIRCLE_SEGMENTS; i++) {
		angle = 2.0 * M_PI * (float)i / (float)CIRCLE_SEGMENTS;
		x = r * cosf(angle);
		y = r * sinf(angle);
		glVertex2f(x + cx, y + cy);
	}
	glEnd();
}


void drawLine(float x0, float y0, float x1, float y1, float thickness)
{
	drawRectangle(x0,y0,x1,y1,thickness);
}


void drawRectangle(float x0, float y0, float x1, float y1, float thickness)
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
    glBegin(GL_QUADS);
	glVertex2f(x0l, y0l);
	glVertex2f(x0r, y0r);
	glVertex2f(x1r, y1r);
	glVertex2f(x1l, y1l);
	glEnd();
}


void drawBullet(struct bullet b)
{
	float r = (1./2. + b.charge) / 12.;
	float x = cos(M_PI/6.) * r;
	float y = sin(M_PI/6.) * r;
	float lx = - x, ly = - y;
	float rx = + x, ry = - y;
	float bx =   0, by = - 2*r;
	drawCircle(b.x, b.y, r);
	rotate(&lx, &ly, b.dir);
	rotate(&rx, &ry, b.dir);
	rotate(&bx, &by, b.dir);
	glBegin(GL_TRIANGLES);
	glVertex2f(b.x + lx,b.y + ly);
	glVertex2f(b.x + rx,b.y + ry);
	glVertex2f(b.x + bx,b.y + by);
	glEnd();
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
	}
	drawBase(t);
	drawTurret(t);
	drawRadar(t);
	if (t.integrity > 0)
		drawIntegrityBar(t);
	glColor(t.bullet_colour);
	for (i=0; i<t.n_bullets; i++)
		drawBullet(t.bullets[i]);
	if (t.integrity > 0)
		drawScan(t);
}


void drawIntegrityBar(struct tank t)
{
	const float length = 5./6.;
	float y  =   9./12.,
	      x0 = - length / 2.,
		  x1 =   x0 + t.integrity * length;
	glColor4f(1.,1.,1.,2./3.);
	drawLine(t.x+x0,t.y+y,t.x+x1,t.y+y,2./30.);
}


void drawRadar(struct tank t)
{
	float flx=-1./6., fly=+1./24., frx=+1./6., fry=+1./24.,
	      bx=.0, by=-1./12.;
	glColor(t.radar_colour);
	rotate(&flx, &fly, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&frx, &fry, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&bx,  &by,  t.radar_dir + t.turret_dir + t.base_dir);
	glBegin(GL_TRIANGLES);
	glVertex2f(t.x + flx, t.y + fly);
	glVertex2f(t.x + frx, t.y + fry);
	glVertex2f(t.x + bx,  t.y + by);
	glEnd();
}


void drawScan(struct tank t)
{
	float fx=0, fy=t.scan_dist+0.5, bx=0, by=0.5;
	rotate(&fx, &fy, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&bx, &by, t.radar_dir + t.turret_dir + t.base_dir);
	t.scan_colour.a = 1./3.; glColor(t.scan_colour);
	drawLine(t.x + bx, t.y + by, t.x + fx, t.y + fy, 1./30.);
	t.scan_colour.a = 1./1.; glColor(t.scan_colour);
	drawCircle(t.x + fx, t.y + fy, 1./30.);
}


void drawTurret(struct tank t)
{
	float flx=-3./48., fly=+7./12., frx=+3./48., fry=+7./12.,
	      blx=-2./24., bly=+.0,     brx=+2./24., bry=+.0;
	glColor(t.turret_colour);
	drawCircle(t.x, t.y, 13./48.);
	rotate(&flx,&fly,t.turret_dir+t.base_dir);
	rotate(&frx,&fry,t.turret_dir+t.base_dir);
	rotate(&blx,&bly,t.turret_dir+t.base_dir);
	rotate(&brx,&bry,t.turret_dir+t.base_dir);
    glBegin(GL_QUADS);
    glVertex2f(t.x + flx, t.y + fly);
    glVertex2f(t.x + frx, t.y + fry);
    glVertex2f(t.x + brx, t.y + bry);
    glVertex2f(t.x + blx, t.y + bly);
    glEnd();
}


void drawBase(struct tank t)
{
	float lflx=-11./24., lfly=+1./2., lfrx=- 6./24., lfry=+1./2.,
	      lblx=-11./24., lbly=-1./2., lbrx=- 6./24., lbry=-1./2.,
	      rflx=+ 6./24., rfly=+1./2., rfrx=+11./24., rfry=+1./2.,
	      rblx=+ 6./24., rbly=-1./2., rbrx=+11./24., rbry=-1./2.;

	rotate(&lflx, &lfly, t.base_dir);
	rotate(&lfrx, &lfry, t.base_dir);
	rotate(&lblx, &lbly, t.base_dir);
	rotate(&lbrx, &lbry, t.base_dir);
	rotate(&rflx, &rfly, t.base_dir);
	rotate(&rfrx, &rfry, t.base_dir);
	rotate(&rblx, &rbly, t.base_dir);
	rotate(&rbrx, &rbry, t.base_dir);

	glColor(t.track_colour);
    glBegin(GL_QUADS);
    glVertex2f(t.x + lflx, t.y + lfly);
    glVertex2f(t.x + lfrx, t.y + lfry);
    glVertex2f(t.x + lbrx, t.y + lbry);
    glVertex2f(t.x + lblx, t.y + lbly);
    glEnd();

    glBegin(GL_QUADS);
    glVertex2f(t.x + rflx, t.y + rfly);
    glVertex2f(t.x + rfrx, t.y + rfry);
    glVertex2f(t.x + rbrx, t.y + rbry);
    glVertex2f(t.x + rblx, t.y + rbly);
    glEnd();

	glColor(t.base_colour);
	drawCircle(t.x, t.y, 1./2.);
}


void drawField(struct field field)
{
	int i;
	float f;
	glColor(field.colour);
	glBegin(GL_QUADS);
	glVertex2f(0.,          0.);
	glVertex2f(0.,          field.height);
	glVertex2f(field.width, field.height);
	glVertex2f(field.width, 0.);
	glEnd();
	glColor4f(0.,0.,0.,1./2.);
	// field.obstacle_colour.a = 1./6.; glColor(field.obstacle_colour);
	for(f=0.; f <= field.height; f+=1.) {
		drawLine(0.,f,field.width,f,3./60.);
	}
	for(f=0.; f <= field.width; f+=1.) {
		drawLine(f,0.,f,field.height,3./60.);
	}
	field.obstacle_colour.a = 1.;
	glColor(field.obstacle_colour);
	for (i=0; i<field.n_obstacles; i++)
		drawObstacle(field.obstacles[i]);
}


void drawObstacle(struct obstacle obstacle)
{
	glBegin(GL_TRIANGLES);
	glVertex2f(obstacle.x1, obstacle.y1);
	glVertex2f(obstacle.x2, obstacle.y2);
	glVertex2f(obstacle.x3, obstacle.y3);
	glEnd();
}
