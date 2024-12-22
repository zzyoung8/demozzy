package com.young.sky.Service.Impl;

import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.young.sky.Entity.Item;
import com.young.sky.Entity.Message;
import com.young.sky.Service.ItemService;
import com.young.sky.Service.PayService;
import com.young.sky.Service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.AmqpException;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import java.sql.Wrapper;
import java.time.LocalDateTime;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
public class PayServiceImpl implements PayService {

    private final StringRedisTemplate stringRedisTemplate;

    public PayServiceImpl(StringRedisTemplate stringRedisTemplate){
        this.stringRedisTemplate = stringRedisTemplate;
    }


    @Autowired
    private UserService userService;

    @Autowired
    private ItemService itemService;

    @Autowired
    private RabbitTemplate rabbitTemplate;

    private static final String TICKET_SOCKET = "ticket:socket";
    private static final String BOUGHT_USER = "bought:user";



    @PostConstruct   // 在构造函数之后，业务代码之前执行
    public void initSocket(){
        // 使用数据库里数据库存
        Item item = itemService.getById(4);
        if(Boolean.FALSE.equals(stringRedisTemplate.hasKey(TICKET_SOCKET))){
            stringRedisTemplate.opsForValue().set(TICKET_SOCKET, item.getSocket().toString());
        }
    }

    @Transactional
    public boolean payTicket(Integer id){

        String username = userService.getById(id).getName();

        // 1. 判断是否已经抢到票
        String userKey = BOUGHT_USER + id;

        if(Boolean.TRUE.equals(stringRedisTemplate.hasKey(userKey))){
            log.info("用户{}已经下单！", username);
            return false;
        }

        // 2. redis扣减库存     mysql 没有减少且 redis扣减了但明明报错了
        Long count = stringRedisTemplate.opsForValue().decrement(TICKET_SOCKET);
        // 2.1 更新数据库，扣减库存：方法一
        itemService.update().eq("id", 4).setSql("socket = socket - 1");
        // 方法二
//        UpdateWrapper<Item> wrapper = new UpdateWrapper<>();
//        wrapper.eq("id", 4).setSql("socket = socket - 1");
//        itemService.update(wrapper);

        // 3. 说明这个存库不够了，返回-1
        if (count == null){
            log.info("TicketKEY不存在");
            return false;
        }
        if(count < 0){
            stringRedisTemplate.opsForValue().increment(TICKET_SOCKET);
            log.info("库存不足，已经将库存加回");
            return false;
        }

        // 4.异步下单
        try {
            Message message = new Message();
            message.setUsername(username);
            message.setCreateTime(LocalDateTime.now());
            message.setItemName(itemService.getById(4).getName());

            // 交由消息队列处理
            rabbitTemplate.convertAndSend("ticket.dir.exchange", "ticket.order", JSONUtil.toJsonStr(message));

            stringRedisTemplate.opsForValue().set(userKey, "1", 2, TimeUnit.MINUTES);

            log.info("消息异步发送成功！");
            return true;

        } catch (AmqpException e) {
            stringRedisTemplate.opsForValue().increment(TICKET_SOCKET);
            log.error("消息发送失败");
            throw new RuntimeException(e);
        }
    }
}
