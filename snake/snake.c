#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define UP 0
#define RIGHT 1
#define DOWN 2
#define LEFT 3

#define CLOCKWISE 0
#define COUNTERCLOCKWISE 1

struct snakeDot {
	int x, y;
	struct snakeDot *next, *prev;
};

struct board {
	int width, height;
};

struct snake {
	struct snakeDot *head, *tail;
	/* CLOCKWISE/COUNTERCLOCKWISE */
	int direction;
	/* UP/RIGHT/DOWN/LEFT */
	int currentDirection;
	int size;
	struct board* board;
};

struct snake* getSnake(int,int,int);
void clean(struct snake*);
void move(struct snake*);
void changeDirection(struct snake*);
void display(struct snake*);

int main(int argc, char* argv[]) {

	int i;

	struct snake *snake = getSnake(3, 8, 8);

	for(i=0;i<30;++i) {
		move(snake);
		display(snake);
		usleep(500000);
	}

	clean(snake);

	return 0;
}

struct snake* getSnake(int size, int boardWidth, int boardHeight) {
	int i;
	struct snake* snake;
	struct snakeDot *head;
	struct board *board;
	struct snakeDot *prev, *curr;

	snake = malloc(sizeof(struct snake));
	head = malloc(sizeof(struct snakeDot));
	board = malloc(sizeof(struct board));

	head->x=0;
	head->y=0;
	prev = head;
	for(i=0;i<size-1;++i) {
		curr = malloc(sizeof(struct snakeDot));
		prev->next = curr;
		curr->prev = prev;
		curr->x = prev->x-1;
		curr->y = prev->y-1;
		prev = curr;
	}
	snake->tail = prev;

	board->width = boardWidth;
	board->height = boardHeight;

	snake->direction = CLOCKWISE;
	snake->currentDirection = RIGHT;
	snake->size = size;
	snake->head = head;
	snake->board = board;
	return snake;
}

void clean(struct snake *snake) {
	struct snakeDot *curr,*next;
	curr = snake->head;
	while(curr!=NULL) {
		next = curr->next;
		free(curr);
		curr = next;
	}
	free(snake->board);
	free(snake);
}

void changeDirection(struct snake *snake) {
	switch(snake->currentDirection) {
		case RIGHT: {
			if(snake->head->x >= snake->board->width-1) {
				snake->head->x = snake->board->width-1;
				snake->currentDirection = (snake->direction==CLOCKWISE) ?
						DOWN : UP ;
			}
			break;
		}
		case UP: {
			if(snake->head->y <= 0) {
				snake->head->y = 0;
				snake->currentDirection = (snake->direction==CLOCKWISE) ?
						RIGHT : LEFT ;
			}
			break;
		}
		case DOWN: {
			if(snake->head->y >= snake->board->height-1) {
				snake->head->y = snake->board->height-1;
				snake->currentDirection = (snake->direction==CLOCKWISE) ?
						LEFT : RIGHT ;
			}
			break;
		}
		case LEFT: {
			if(snake->head->x <= 0) {
				snake->head->x = 0;
				snake->currentDirection = (snake->direction==CLOCKWISE) ?
						UP : DOWN ;
			}
			break;
		}
	}
}

void move(struct snake *snake) {
	struct snakeDot *curr;

	curr = snake->tail;
	while(curr!=snake->head) {
		curr->x = curr->prev->x;
		curr->y = curr->prev->y;
		curr = curr->prev;
	}

	switch(snake->currentDirection) {
		case RIGHT: {
			++snake->head->x;
			break;
		}
		case UP: {
			--snake->head->y;
			break;
		}
		case DOWN: {
			++snake->head->y;
			break;
		}
		case LEFT: {
			--snake->head->x;
			break;
		}
	}

	changeDirection(snake);
}

int getFieldCode(struct snake *snake, int x, int y) {
	struct snakeDot *curr = snake->head;
	while(curr!=NULL) {
		if(curr->x == x && curr->y == y) return 1;
		curr = curr->next;
	} 
	return 0;
}

void display(struct snake *snake) {
	int i,j;
	system("clear");
	for(i=0;i<snake->board->width;++i) {
		for(j=0;j<snake->board->height;++j) {
			printf("%d", getFieldCode(snake,i,j));
		}
		printf("\n");
	}
}