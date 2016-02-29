#include <stdio.h>
#include <stdlib.h>
#include <linux/ipc.h>
#include <linux/msg.h>
#include <errno.h>
#include <string.h>

typedef struct
{
 int sender;
 int recipient;
 char text[160];
 int service;
 int service_data;
} message_t;

typedef struct
{
 long mtype;
 message_t mtext;
} messagebuf_t;

void set_sender(messagebuf_t *buf, int sender);
void set_recipient(messagebuf_t *buf, int recipient);
void set_text(messagebuf_t *buf, char *text);
void set_service(messagebuf_t *buf, int service);
void set_service_data(messagebuf_t *buf, int data);

int get_sender(messagebuf_t *buf);
int get_recipient(messagebuf_t *buf);
void get_text(messagebuf_t *buf, char *text);
int get_service(messagebuf_t *buf);
int get_service_data(messagebuf_t *buf);

void init_message(messagebuf_t *buf);

/* This function creates a unique SysV IPC key */
/* from a letter passed as a parameter */
key_t build_key(char c);

/* This function creates a SysV message queue identified by an IPC key */
/* The returned int is the identifier of the queue */
int create_queue(key_t key);

/*  This function removes the queue from the kernel address space */
int remove_queue(int qid);

/* This function sends a message to the queue identified by qid. */
/* Remember the length of a message excludes the field mtype */
int send_message(int qid, messagebuf_t *qbuf);

/* This function reads a message from the queue qid filtering the field mtype */
/* i.e. gets from the queue the first message with the filed mtype set to the vaule of type */
int receive_message(int qid, long type, messagebuf_t *qbuf);
