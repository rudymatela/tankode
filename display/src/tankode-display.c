#include <stdio.h>
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <math.h>
#include "tankode.h"
#include "draw.h"
#include "io.h"

#define WIDTH 400
#define HEIGHT 400
#define FPS 60 /* TODO: implement VSync */

struct state state = {-1,{8.,6.,0,{}},-1,{}};

float field_proportion() {return state.field.width / state.field.height;}

static void reshape(int w, int h);
static void render();
static void render_and_reschedule(int val);

int main(int argc, char *argv[])
{
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
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_LOAD,  1./6.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./6.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./6.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./6.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./6.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); update_state(); draw(); glAccum(GL_ACCUM, 1./6.);
	glAccum(GL_RETURN, 1.);
	glutSwapBuffers();
}
