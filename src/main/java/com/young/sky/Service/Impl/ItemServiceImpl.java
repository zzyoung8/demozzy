package com.young.sky.Service.Impl;


import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.young.sky.Entity.Item;
import com.young.sky.Entity.Order;
import com.young.sky.Entity.User;
import com.young.sky.Mapper.ItemMapper;
import com.young.sky.Service.ItemService;
import com.young.sky.Service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.AmqpException;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.security.Key;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
public class ItemServiceImpl extends ServiceImpl<ItemMapper, Item> implements ItemService {

    @Override
    public List<Item> getAllItem() {
        return this.list();
    }
}
