/* Define the type of messages we can handle */

#define MAX_SLEEP 6

#define TYPE_SERVICE   1
#define TYPE_TEXT   2

/* Define the service types */

#define SERVICE_TERMINATE 1
#define SERVICE_TIME 2
#define SERVICE_CONNECT 3
#define SERVICE_DISCONNECT 4
#define SERVICE_QID 5
#define SERVICE_UNREACHABLE_DESTINATION 6

int init_queue();
void close_queue(int qid);

void user_send_connect(int sender, int sw);
void user_send_qid(int sender, int qid, int sw);
void user_send_text_message(int sender, int recipient, char *text, int sw);
void user_send_time(int sender, int sw);
void user_send_disconnect(int sender, int pid, int sw);

void switch_send_text_message(int sender, char *text, int user);
void switch_send_terminate(int qid);
void switch_send_time(int qid);
