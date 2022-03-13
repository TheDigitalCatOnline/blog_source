#include "layer1.h"
#include "errno.h"

void set_type(messagebuf_t * buf, int type){
  buf->mtype = type;
}

int get_sender(messagebuf_t *buf){
  return buf->mtext.sender;
}

void set_sender(messagebuf_t *buf, int sender){
  buf->mtext.sender = sender;
}

void set_recipient(messagebuf_t *buf, int recipient){
  buf->mtext.recipient = recipient;
}

int get_recipient(messagebuf_t *buf){
  return buf->mtext.recipient;
}

void set_text(messagebuf_t *buf, char *text){
  strcpy(buf->mtext.text, text);
}

void get_text(messagebuf_t *buf, char *text){
  strcpy(text, buf->mtext.text);
}

int get_service(messagebuf_t *buf){
  return buf->mtext.service;
}

void set_service(messagebuf_t *buf, int service){
  buf->mtext.service = service;
}

void set_service_data(messagebuf_t *buf, int service_data){
  buf->mtext.service_data = service_data;
}

int get_service_data(messagebuf_t *buf){
  return buf->mtext.service_data;
}

void init_message(messagebuf_t *buf){
  buf->mtext.sender = -1;
  buf->mtext.recipient = -1;
  strcpy(buf->mtext.text, "");
  buf->mtext.service = -1;
  buf->mtext.service_data = -1;
}

/* This function creates a unique SysV IPC key */
/* from a character passed as a parameter */
key_t build_key(char c){
  key_t key;
  key = ftok(".", c);
  return key;
}

/* This function creates a SysV message queue identified by an IPC key */
/* The returned int is the identifier of the queue */
int create_queue(key_t key){
  int qid;
  
  if((qid = msgget(key, IPC_CREAT | 0660)) == -1){
    perror("msgget");
    exit(1);
  }

  return qid;
}

/*  This function removes the queue from the kernel address space */
int remove_queue(int qid){
  if(msgctl(qid, IPC_RMID, 0) == -1)
  {
    perror("msgctl");
    exit(1);
  }
  return 0;
}

/* This function sends a message to the queue identified by qid. */
/* Remember the length of a message excludes the field mtype */
int send_message(int qid, messagebuf_t *qbuf){
  int result, lenght;
  lenght = sizeof(messagebuf_t) - sizeof(long);
  
  if ((result = msgsnd(qid, qbuf, lenght, 0)) == -1){
    perror("msgsnd");
    exit(1);
  }
  
  return result;
}

/* This function reads a message from the queue qid filtering the field mtype */
/* i.e. gets from the queue the first message with of given type */
int receive_message(int qid, long type, messagebuf_t *qbuf){
  int result, length;
  length = sizeof(messagebuf_t) - sizeof(long);
  
  if((result = msgrcv(qid, (struct msgbuf *)qbuf, length, type, IPC_NOWAIT)) == -1){
    if(errno == ENOMSG){
      return 0;
    }
    else{
      perror("msgrcv");
      exit(1);
    }
  }
  
  return result;
}
