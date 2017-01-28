#include <math.h>
#include "draw.h"

#define CIRCLE_SEGMENTS 30

/* rotate given x and y destructively */
void rotate(float *x, float *y, float dir)
{
	float x0 = *x,
	      y0 = *y;
	*x = x0 * cos(dir) - y0 * sin(dir);
	*y = x0 * sin(dir) + y0 * cos(dir);
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


void drawTank(struct tank t)
{
	int i;
	drawBase(t);
	drawTurret(t);
	drawRadar(t);
	glColor(t.bullet_colour);
	for (i=0; i<t.n_bullets; i++)
		drawBullet(t.bullets[i]);
	drawScan(t);
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
	float fx=0, fy=30, bx=0, by=0.5;
	rotate(&fx, &fy, t.radar_dir + t.turret_dir + t.base_dir);
	rotate(&bx, &by, t.radar_dir + t.turret_dir + t.base_dir);
	glColor(t.scan_colour);
	glBegin(GL_LINES);
	glVertex2f(t.x + bx, t.y + by);
	glVertex2f(t.x + fx, t.y + fy);
	glEnd();
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
		glBegin(GL_LINES);
		glVertex2f(0.         ,f);
		glVertex2f(field.width,f);
		glEnd();
	}
	for(f=0.; f <= field.width; f+=1.) {
		glBegin(GL_LINES);
		glVertex2f(f, 0.          );
		glVertex2f(f, field.height);
		glEnd();
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
