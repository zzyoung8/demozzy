package com.young.sky.Service.Impl;

import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.young.sky.Entity.Message;
import com.young.sky.Entity.Order;
import com.young.sky.Mapper.OrderMapper;
import com.young.sky.Service.OrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Slf4j
@Service
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements OrderService {

    @RabbitListener(bindings = @QueueBinding(
            value = @Queue("ticket.order.queue"),
            exchange = @Exchange("ticket.dir.exchange"),
            key = "ticket.order"
    ))
    public void generateOrder(String messageJson){
        try {
            log.info("消息收到了！");
            Message message = JSONUtil.toBean(messageJson, Message.class);
            Order order = new Order();
            order.setUsername(message.getUsername());
            order.setItemName(message.getItemName());
            order.setCreateTime(LocalDateTime.now());

            save(order);
            log.info("订单创建成功{}", order.getId());
        } catch (Exception e) {
            log.error("订单创建失败");

            // 可加入重试机制（有关mq）
            throw new RuntimeException(e);
        }
    }
}
