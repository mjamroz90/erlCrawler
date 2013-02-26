/*
 * parse_html_driver.cpp
 *
 *  Created on: 02-02-2013
 *      Author: mjamroz
 */

#include "parse_html.h"
#include <erl_driver.h>
#include <ei.h>

#define MAX_URL_SIZE 1024

static ErlDrvData drv_start(ErlDrvPort port, char *command);

static void drv_stop(ErlDrvData handle);

static void drv_output(ErlDrvData handle, char *buf, ErlDrvSizeT sz);

static ei_x_buff make_error(const char* text);

static ei_x_buff pack_parse_data(parse_result parse_result_);

static ErlDrvEntry parse_html_driver_entry = {
    NULL,                            /* init */
    drv_start,                       /* start */
    drv_stop,                        /* stop */
    drv_output,                      /* output */
    NULL,                            /* ready_input */
    NULL,                            /* ready_output */
    "parse_html_driver",               /* driver_name */
    NULL,                            /* finish */
    NULL,                            /* handle (reserved) */
    NULL,                            /* control */
    NULL,                            /* timeout */
    NULL,                            /* outputv */
    NULL,                            /* ready_async */
    NULL,                            /* flush */
    NULL,                            /* call */
    NULL,                            /* event */
    ERL_DRV_EXTENDED_MARKER,         /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,  /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,  /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING    /* ERL_DRV_FLAGs */
};

extern "C"{

	DRIVER_INIT(parse_html_driver){
		return &parse_html_driver_entry;
	}
}

typedef struct{
	ErlDrvPort port;
} drv_data_t;

struct decode_result{

	bool decode_success;
	std::pair<std::string,std::string> html_content;
	const char* error_msg;
};

decode_result process_data(ErlDrvData handle,unsigned char* buf, ErlDrvSizeT* sz,char*& html_content,char*& url_address);

//------------------------------------Callbacks-------------------------------

static ErlDrvData drv_start(ErlDrvPort port, char *command){

	drv_data_t* d = (drv_data_t*)driver_alloc(sizeof(drv_data_t));
	d->port = port;
	return (ErlDrvData)d;

}

static void drv_stop(ErlDrvData handle){
	driver_free((char*)handle);
}

static void drv_output(ErlDrvData handle, char *buf,ErlDrvSizeT sz){

	ei_x_buff packed_data;
	char* html_content = NULL;
	char* url_address = NULL;
	//first - url, second - html content
	decode_result process_data_result = process_data(handle,(unsigned char*)buf,&sz,html_content,url_address);
	if (!process_data_result.decode_success){
		packed_data = make_error(process_data_result.error_msg);
	}
	else{
	//first - links, second - wholeText
		parse_result process_html_result =
				process_html_source(process_data_result.html_content.second,process_data_result.html_content.first);
		packed_data = pack_parse_data(process_html_result);
		if (html_content != NULL && url_address != NULL){
			driver_free(html_content);
			driver_free(url_address);
		}
	}
	drv_data_t* drv_t = (drv_data_t*)handle;
	driver_output(drv_t->port, packed_data.buff, packed_data.buffsz);
	ei_x_free(&packed_data);

}


decode_result process_data(ErlDrvData handle,unsigned char* buf, ErlDrvSizeT* sz,char*& html_content,char*& url_address){

	int index = 0,size=0,type=0,ver=0;
	if (ei_decode_version((char*)buf, &index,&ver)){
		//data encoding version mismatch
		return decode_result{false,std::pair<std::string,std::string>("",""),
			"data encoding version mismatch"};
	}
	else if (ei_get_type((char*)buf,&index,&type,&size)){
		//must be a binary
		return decode_result{false,std::pair<std::string,std::string>("",""),
			"must be a binary"};
	} else{
		int tuple_arity;
		ei_decode_tuple_header((char*)buf,&index,&tuple_arity);
		ei_get_type((char*)buf,&index,&type,&size);
		url_address = (char*)driver_alloc((size+2)*sizeof(char));
		ei_decode_string((char*)buf,&index,url_address);
		ei_get_type((char*)buf,&index,&type,&size);
		//cout << "Html Content Size " << size << endl;
		html_content = (char*)driver_alloc((size+2)*sizeof(char));
		ei_decode_string((char*)buf,&index,html_content);
		*sz = size;
		std::string url_address_str(url_address);
		std::string html_content_str(html_content);
		return decode_result{true,std::pair<std::string,std::string>(url_address_str,html_content_str),NULL};
	}
}


static ei_x_buff make_error(const char* text){

	ei_x_buff x;
	ei_x_new_with_version(&x);
	ei_x_encode_tuple_header(&x,2);
	ei_x_encode_atom(&x,"error");
	ei_x_encode_string(&x,text);

	return x;
}

static ei_x_buff pack_parse_data(parse_result parse_result_){

	ei_x_buff x;
	ei_x_new_with_version(&x);
	ei_x_encode_tuple_header(&x,2);
	//{caly_tekst,[link]} kodujemy caly tekst
	ei_x_encode_string(&x, parse_result_.second.c_str());
	int link_list_length = parse_result_.first.size();
	ei_x_encode_list_header(&x,link_list_length);
	vector<std::string>& links = parse_result_.first;
	for (vector<std::string>::iterator it = links.begin(); it != links.end(); it++){
		ei_x_encode_string_len(&x,(*it).c_str(),(*it).size());
	}
	ei_x_encode_empty_list(&x);

	return x;
}
