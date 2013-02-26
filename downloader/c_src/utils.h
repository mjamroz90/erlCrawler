#include <set>
#include <string>
#include <string.h>
#include <vector>

#ifndef UTILS_H_
#define UTILS_H_

struct valid_tags_{
	private:

		struct insensitiveCompare{
			bool operator() (const std::string& a,const std::string& b) const{
				return strcmp(a.c_str(),b.c_str()) < 0;
			}
		};

		std::set<std::string,insensitiveCompare>* _valid_tags;
	public:

		valid_tags_(){
			static const std::string valid_tags_array[] = {
					"head",
					"script",
					"link",
					"style",
					"meta",
					"img"
			};

			_valid_tags = new std::set<std::string,insensitiveCompare>
									(valid_tags_array,valid_tags_array+6);

		}

		bool contains(std::string str) const{
			return (_valid_tags->find(str) != _valid_tags->end());
		}

} valid_tags;


template <typename T,typename P> T remove_if(T beg,T end,P pred){

	T result = beg;
	for (T it = beg; it != end; it++){
		if (!pred(*it)){
			*(result++) = *it;
		}
	}
	return result;
}

template <typename T,typename Pred> bool is_all_true(T beg, T end,Pred pred){
	for (T it = beg; it != end; it++){
		if (!pred(*it)){
			return false;
		}
	}
	return true;
}

typedef std::pair<std::vector<std::string>, std::string> parse_result;

const std::string join_urls_parts(std::string base_url,std::string href_attr_val){

	if ((href_attr_val.find("http://") != std::string::npos) ||
			(href_attr_val.find("https://") != std::string::npos)){
		return href_attr_val;
	}

	std::string result;
	int first_slash_pos = base_url.find("/",7);
	result.append(base_url.substr(0,first_slash_pos));
	if (href_attr_val[0] != '/'){
		result.append("/");
	}
	result.append(href_attr_val);

	return result;

}

const bool is_link_to_other_page(std::string link){
	unsigned int slash_pos = link.rfind("/");
	if (slash_pos != std::string::npos){
		return link[slash_pos+1] != '#';
	}
	else{
		return (link[0] != '#');
	}
}

#endif /* UTILS_H_ */
