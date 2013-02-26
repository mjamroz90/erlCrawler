/*
 * parse_html.cpp
 *
 *  Created on: 02-02-2013
 *      Author: mjamroz
 */
#include "utils.h"

#include <htmlcxx/html/ParserDom.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <ctype.h>

#define STRING_SIZE 16384
#define URLS_SIZE 4096

using namespace std;
using namespace htmlcxx;

//sprawdza, czy wchodzimy do tego taga.
const bool is_valid_tag(HTML::Node* node){

	std::string tagName = node->tagName();
	return !valid_tags.contains(tagName);
}

const inline bool is_tag(HTML::Node* node){
	return (node->isTag()) && !(node->isComment());
}

struct _is_space{
	bool operator() (const char c) const {
		return (c == '\t') || (c == ' ') || (c == '\n');
	}
} is_space;

const inline std::pair<bool,std::string> get_url(HTML::Node* node){
	node->parseAttributes();
	return node->attribute("href");
}

const parse_result process_html_source(std::string html,std::string base_url){

	HTML::ParserDom parser;
	parse_result result;
	tree<HTML::Node> dom = parser.parseTree(html);
	tree<HTML::Node>::iterator it = dom.begin();
	tree<HTML::Node>::iterator end = dom.end();

	std::string wholeText;
	wholeText.reserve(STRING_SIZE);
	std::vector<std::string> links;
	links.reserve(URLS_SIZE);

	for (; it != end; ){
			if (is_tag(&(*it))){

				if (is_valid_tag(&(*it))){
					// przetwarzamy taga
					if (it->tagName() == "a" || it->tagName() == "A"){
						std::pair<bool,std::string> attr = get_url(&(*it));
						if (attr.first && is_link_to_other_page(attr.second)){
							links.push_back(join_urls_parts(base_url,attr.second));
						}
					}
				}
				else{
					it.skip_children();
				}

			}
			else if (!it->isComment()){
				//mamy tekst do dodania
				std::string it_text = it->text();
				if (!is_all_true(it_text.begin(),it_text.end(),is_space)){
					wholeText += it->text();
				}
			}
			it++;
	}

	result.first = links;
	result.second = wholeText;
	return result;
}

