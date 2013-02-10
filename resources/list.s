(contents
    
list  
(class list [_0]
 {begin ((method [] (iterator _0)))  
  end ((method [] (iterator _0)))
  rbegin ((method [] (reverse_iterator _0)))
  rend ((method [] (reverse_iterator _0)))
  cbegin ((method [] (const_iterator _0)))
  cend ((method [] (const_iterator _0)))
  crbegin ((method [] (const_reverse_iterator _0)))
  crend ((method [] (const_reverse_iterator _0)))
  
  empty ((method [] boolean))
  size ((method [] uint))
  max_size ((method [] uint))
  
  front ((method [] _0))
  back ((method [] _0))
  
  ;assign ((method [_0 _1] void))
  ;emplace_front 
  push_front ((method [_0] void))
  pop_front ((method [] void))
  ;emplace_back 
  push_back ((method [_0] void))
  pop_back ((method [] void))
  ;emplace
  ;insert
  erase ((method [(iterator _0)] (iterator _0)))
  swap ((method [(list _0)] void))
  resize ((method [uint _0] void))
  clear ((method [] void))
  
  ;splice
  ;remove
  ;remove_if
  ;unique
  ;merge
})

)