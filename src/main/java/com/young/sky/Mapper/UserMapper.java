package com.young.sky.Mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.young.sky.Entity.Item;
import com.young.sky.Entity.User;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface UserMapper extends BaseMapper<User> {}
