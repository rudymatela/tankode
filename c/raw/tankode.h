#ifndef _TANKODE_H
#define _TANKODE_H

#define MAX_NAME 0x100

struct colour {
	float r,g,b;
};

struct tankode_id {
	char name[MAX_NAME];
	struct colour
	track,
	body,
	gun,
	radar,
	bullet,
	scan;
};

struct tankode_in {
	double integrity;
	double speed;
	double enemy;
	double wall;
};

struct tankode_out {
	double
	accel,
	body,
	gun,
	radar,
	shoot;
};

void tankode_run(struct tankode_id id, struct tankode_out tankode(struct tankode_in in));

#endif /* _TANKODE_H */
