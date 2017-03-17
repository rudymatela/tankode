#ifndef _TANKODE_H
#define _TANKODE_H

struct tankode_id {
	char *name;
	char *track;
	char *body;
	char *gun;
	char *radar;
	char *buller;
	char *scan;
};

struct tankode_in {
	double integrity;
	double speed;
	double enemy;
	double wall;
	int scanned_enemy;
	int scanned_wall;
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
