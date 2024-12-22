package com.young.sky.Service.Impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.young.sky.Entity.Order;
import com.young.sky.Entity.User;
import com.young.sky.Mapper.OrderMapper;
import com.young.sky.Mapper.UserMapper;
import com.young.sky.Service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

}
