/* tankode-display.c -- displayer of the Tankode game
 *
 * Copyright (C) 2016, 2017  Rudy Matela
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
#include <stdio.h>
#include <GL/freeglut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <math.h>
#include <string.h>
#include <libgen.h>
#include "tankode.h"
#include "draw.h"
#include "io.h"
#include "tankode-audio.h"

#define DEFAULT_WINDOW_WIDTH 800
#define DEFAULT_WINDOW_HEIGHT 600
#define FPS 60 /* TODO: implement VSync */

int motion_blur = 1;
int draw_charge = 0;
int draw_health = 1;
int draw_scan = 1;
int dump_frames = 0;
int window_width  = DEFAULT_WINDOW_WIDTH;
int window_height = DEFAULT_WINDOW_HEIGHT;
int close_window = 0;
int sound = 1;

int window;

struct state state = {-1,{8.,6.,0,{}},-1,{}};

float field_proportion() {return state.field.width / state.field.height;}

static void reshape(int w, int h);
static void render();
static void render_and_reschedule(int val);
static void close();
static void update_ortho();

void parse_args(char *argv[])
{
	int i, r, w, h;
	for (i=0; argv[i]; i++) {
		if (0==strcmp(argv[i],"motion-blur"))     motion_blur  = 1;
		if (0==strcmp(argv[i],"no-motion-blur"))  motion_blur  = 0;
		if (0==strcmp(argv[i],"draw-charge"))     draw_charge  = 1;
		if (0==strcmp(argv[i],"no-draw-charge"))  draw_charge  = 0;
		if (0==strcmp(argv[i],"draw-health"))     draw_health  = 1;
		if (0==strcmp(argv[i],"no-draw-health"))  draw_health  = 0;
		if (0==strcmp(argv[i],"draw-scan"))       draw_scan    = 1;
		if (0==strcmp(argv[i],"no-draw-scan"))    draw_scan    = 0;
		if (0==strcmp(argv[i],"dump-frames"))     dump_frames  = 1;
		if (0==strcmp(argv[i],"no-dump-frames"))  dump_frames  = 0;
		if (0==strcmp(argv[i],"close-window"))    close_window = 1;
		if (0==strcmp(argv[i],"no-close-window")) close_window = 0;
		if (0==strcmp(argv[i],"sound"))           sound        = 1;
		if (0==strcmp(argv[i],"no-sound"))        sound        = 0;
		if (0==strncmp(argv[i],"window-size=",12)) {
			r = sscanf(argv[i]+12,"%ix%i",&w,&h);
			if (r != 2)
				fprintf(stderr,"could not parse screen size %s\n",argv[i]);
			window_width  = w;
			window_height = h;
		}
	}
}

int main(int argc, char *argv[])
{
	setlinebuf(stdin);
	parse_args(argv);
	state = get_initial_state();
	read_tick(&state);
	glutInit(&argc, argv);
	glutInitContextVersion(2, 1);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_MULTISAMPLE | GLUT_DEPTH);
	glutInitWindowSize(window_width, window_height);
	window = glutCreateWindow("tankode");
	update_ortho();
	glEnable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glClearColor(state.field.obstacle_colour.r, state.field.obstacle_colour.g, state.field.obstacle_colour.b, 1.);
	GLenum error = glGetError();
	if (error != GL_NO_ERROR)
	{
		printf("Error initializing OpenGL! %s\n", gluErrorString(error));
		return -1;
	}
	glutReshapeFunc(reshape);
	glutDisplayFunc(render);
	glutCloseFunc(close);
	glutTimerFunc(1000 / FPS, render_and_reschedule, 0);
	if (sound) {
		tankode_audio_init(
			dirname(argv[0]),
			state.field.width/2,
			state.field.height/2,
			state.field.width > state.field.height ?
				state.field.width :
				state.field.height
		);
	}
	glutMainLoop();
	return 0;
}

static void update_ortho()
{
	static float ortho_w = 0.;
	static float ortho_h = 0.;
	if (state.field.width  == ortho_w
	 && state.field.height == ortho_h)
		return;
	ortho_w = state.field.width;
	ortho_h = state.field.height;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glOrtho(0., ortho_w, 0., ortho_h, -1., 1.);
	reshape(glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT));
}

static void close()
{
	tankode_audio_finalize();
	exit(0);
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

/* returns 1 when it needs to stop */
int update_state()
{
	static int reading = 1;
	if (reading) {
		reading = read_tick(&state);
		update_ortho();
	} else if (close_window) {
		glutDestroyWindow(window);
		close();
	}
	return 0;
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
} else {
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
