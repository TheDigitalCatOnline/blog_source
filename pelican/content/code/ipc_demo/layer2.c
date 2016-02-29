#include "layer1.h"
#include "layer2.h"
#include <time.h>

/*
 * Queue initialization.
 * This function creates an IPC key and build a message queue.
 */
int init_queue(int num)
{
  key_t key;
  int qid;
    
  key = build_key((char) num);
  qid = create_queue(key);

  return qid;
}

/*
 * Queue deletion.
 * This function removes a queue.
 */
void close_queue(int qid)
{
  remove_queue(qid);
}

/*
 * Connect message (user).
 * This function sends a message to the switch notifying that the user connected to the system.
 */
void user_send_connect(int sender, int sw)
{
  messagebuf_t message;
  
  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_sender(&message, sender);
  set_service(&message, SERVICE_CONNECT);
  send_message(sw, &message);
}

/*
 * Queue ID message (user).
 * This function sends a message to the switch notifying the id of the user's queue.
 */
void user_send_qid(int sender, int qid, int sw)
{
  messagebuf_t message;
  
  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_sender(&message, sender);
  set_service(&message, SERVICE_QID);
  set_service_data(&message, qid);
  send_message(sw, &message);
}

/*
 * Text message (user).
 * This function sends a text message to another user.
 */
void user_send_text_message(int sender, int recipient, char *text, int sw)
{
  messagebuf_t message;

  init_message(&message);
  set_type(&message, TYPE_TEXT);
  set_sender(&message, sender);
  set_recipient(&message, recipient);
  set_text(&message, text);
  send_message(sw, &message);
}

/*
 * Time message (user).
 * This function sends a message to the switch containing the current time.
 */
void user_send_time(int sender, int sw)
{
  messagebuf_t message;
  
  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_sender(&message, sender);
  set_service(&message, SERVICE_TIME);
  set_service_data(&message, (int) time(NULL));
  send_message(sw, &message);
}

/*
 * Disconnect message (user).
 * This function sends a message to the switch notifying that the user disconnected from the system.
 */
void user_send_disconnect(int sender, int pid, int sw)
{
  messagebuf_t message;

  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_sender(&message, sender);
  set_service(&message, SERVICE_DISCONNECT);
  set_service_data(&message, pid);
  send_message(sw, &message);
}

/*
 * Text message (switch).
 * This function sends a text message to a user.
 */
void switch_send_text_message(int sender, char *text, int user)
{
  messagebuf_t message;

  init_message(&message);
  set_type(&message, TYPE_TEXT);
  set_sender(&message, sender);
  set_text(&message, text);
  send_message(user, &message);
}

/*
 * Termination signal (switch).
 * This function sends a message to the user asking to begin the termination procedure.
 */
void switch_send_terminate(int qid)
{
  messagebuf_t message;
  
  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_service(&message, SERVICE_TERMINATE);
  send_message(qid, &message);
}

/* Time request message (switch).
 * This function send a message to a user requesting a timing operation.
 */  
void switch_send_time(int qid)
{
  messagebuf_t message;
  
  init_message(&message);
  set_type(&message, TYPE_SERVICE);
  set_service(&message, SERVICE_TIME);
  send_message(qid, &message);
}

