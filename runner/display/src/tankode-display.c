#include <stdio.h>
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <math.h>
#include <string.h>
#include "tankode.h"
#include "draw.h"
#include "io.h"

#define WIDTH 400
#define HEIGHT 400
#define FPS 60 /* TODO: implement VSync */

int motion_blur = 1;
int draw_charge = 0;
int draw_health = 1;
int draw_scan = 1;
int dump_frames = 0;

struct state state = {-1,{8.,6.,0,{}},-1,{}};

float field_proportion() {return state.field.width / state.field.height;}

static void reshape(int w, int h);
static void render();
static void render_and_reschedule(int val);

void parse_args(char *argv[])
{
	int i;
	for (i=0; argv[i]; i++) {
		if (0==strcmp(argv[i],"motion-blur"))    motion_blur = 1;
		if (0==strcmp(argv[i],"no-motion-blur")) motion_blur = 0;
		if (0==strcmp(argv[i],"draw-charge"))    draw_charge = 1;
		if (0==strcmp(argv[i],"no-draw-charge")) draw_charge = 0;
		if (0==strcmp(argv[i],"draw-health"))    draw_health = 1;
		if (0==strcmp(argv[i],"no-draw-health")) draw_health = 0;
		if (0==strcmp(argv[i],"draw-scan"))      draw_scan = 1;
		if (0==strcmp(argv[i],"no-draw-scan"))   draw_scan = 0;
		if (0==strcmp(argv[i],"dump-frames"))    dump_frames = 1;
		if (0==strcmp(argv[i],"no-dump-frames")) dump_frames = 0;
	}
}

int main(int argc, char *argv[])
{
	parse_args(argv);
	state = get_initial_state();
	print_state(state);
	read_tick(&state);
	print_state(state);
	glutInit(&argc, argv);
	glutInitContextVersion(2, 1);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_MULTISAMPLE | GLUT_DEPTH);
	glutInitWindowSize(WIDTH, HEIGHT);
	glutCreateWindow("tankode");
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glEnable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glOrtho(0., state.field.width, 0.,state.field.height, -1., 1.);
	glClearColor(state.field.obstacle_colour.r, state.field.obstacle_colour.g, state.field.obstacle_colour.b, 1.);
	GLenum error = glGetError();
	if (error != GL_NO_ERROR)
	{
		printf("Error initializing OpenGL! %s\n", gluErrorString(error));
		return -1;
	}
	glutReshapeFunc(reshape);
	glutDisplayFunc(render);
	glutTimerFunc(1000 / FPS, render_and_reschedule, 0);
	glutMainLoop();
	return 0;
}

static void reshape(int w_, int h_)
{
	/* leaves a half-a-tank-radius-sized border */
	int w = 2 * w_ * state.field.width  / (2 * state.field.width  + 1);
	int h = 2 * h_ * state.field.height / (2 * state.field.height + 1);
	/* use: int w = w_, h = h_ for no border */
	int w0 = (w_ - w) / 2;
	int h0 = (h_ - h) / 2;
	float fp = field_proportion();
	float sp = (float)w / (float)h;
	if (sp < fp) {
		glViewport(w0, h0 + (h - w/fp) / 2., w, w/fp);
		return;
	}
	if (sp > fp) {
		glViewport(w0 + (w - h*fp) / 2., h0, h*fp, h);
		return;
	}
	glViewport(w0, h0, w, h);	/* fp == sp */
}

static void draw()
{
	int i;
	drawField(state.field);
	for (i=0; i<state.n_tanks; i++)
		drawTank(state.tanks[i],state.field.obstacle_colour);
	for (i=0; i<state.n_tanks; i++)
		drawExplosions(state.tanks[i]);
}

static void render()
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	draw();
	glutSwapBuffers();
}

void update_state()
{
	static int reading = 1;
	if (reading) {
		reading = read_tick(&state);
	}
}

void render_and_reschedule(int val)
{
	glutTimerFunc(1000 / FPS, render_and_reschedule, val);
if (dump_frames) {
	if (motion_blur) {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_LOAD,  1./4.);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./4.);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./4.);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./4.);
		glAccum(GL_RETURN, 1.);
	} else {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		update_state();
		update_state();
		update_state();
		update_state();
		draw();
	}
} else{
	if (motion_blur) {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_LOAD,  1./2.);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./2.);
		glAccum(GL_RETURN, 1.);
	} else {
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		update_state();
		update_state();
		draw();
	}
}
	if (dump_frames)
		screenshot();
	glutSwapBuffers();
}
