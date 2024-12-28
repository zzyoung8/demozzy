package com.young.sky.Service.Impl;

import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.young.sky.Entity.Item;
import com.young.sky.Entity.Message;
import com.young.sky.Entity.Result;
import com.young.sky.Service.ItemService;
import com.young.sky.Service.PayService;
import com.young.sky.Service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RAtomicLong;
import org.redisson.api.RBucket;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.AmqpException;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.xml.crypto.dsig.spec.ExcC14NParameterSpec;
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

    @Autowired
    private RedissonClient redissonClient;

    private static final String TICKET_SOCKET = "ticket:socket";
    private static final String BOUGHT_USER = "bought:user:";
    private static final String SOCKET_LOCK_KEY = "ticket:socket:lock";



    @PostConstruct   // 在构造函数之后，业务代码之前执行
    public void initSocket(){
        // 使用数据库里数据库存
        Item item = itemService.getById(4);
        if(item != null){
            // RAtomicLong在第一次访问时会自动创建键，并默认初始化为0
            RAtomicLong socketCounter = redissonClient.getAtomicLong(TICKET_SOCKET);
            socketCounter.set(item.getSocket());
        }
        // 下面这个好像是下面代码冲突
//        if(Boolean.FALSE.equals(stringRedisTemplate.hasKey(TICKET_SOCKET))){
//            stringRedisTemplate.opsForValue().set(TICKET_SOCKET, item.getSocket().toString());
//        }
    }

    @Transactional
    @Override
    public Result payTicket(Integer id) {
        String username = userService.getById(id).getName();
        String userKey = BOUGHT_USER + id;

        // 0. 获取分布式锁
        RLock lock = redissonClient.getLock(SOCKET_LOCK_KEY);


        try {
            // 0.5 尝试获取锁，最多等待3秒
            if(!lock.tryLock(3,10, TimeUnit.SECONDS)){
                return Result.fail("系统繁忙，稍后重试");
            }

            // 1. 判断是否已经抢到票
//            if(Boolean.TRUE.equals(stringRedisTemplate.hasKey(userKey))){
//                log.info("用户{}已经下单！", username);
//                return Result.fail("用户已下单");
//            }
            // 1. 判断是否抢到票，这个key对应的是boolean
            RBucket<Boolean> bucket = redissonClient.getBucket(BOUGHT_USER + id);
            if(Boolean.TRUE.equals(bucket.get())){
                return Result.fail("您已下单");
            }

            // 2. 扣减库存，使用客户端方法获取redis里的数值
            RAtomicLong socketCount = redissonClient.getAtomicLong(TICKET_SOCKET);
            long remainingStock = socketCount.decrementAndGet(); // 较少并返回

            try{
                if (remainingStock < 0) {
                    socketCount.incrementAndGet();
                    return Result.fail("库存不足");
                }

                // 3.更新数据库
                UpdateWrapper<Item> updateWrapper = new UpdateWrapper<>();
                updateWrapper.eq("id", 4).setSql("socket = socket - 1");
                boolean isEnough = itemService.update(updateWrapper);
                if(!isEnough){
                    socketCount.incrementAndGet();
                    return Result.fail("库存不足");
                }

                // 4.订单消息
                Message message = new Message();
                message.setUsername(username);
                message.setCreateTime(LocalDateTime.now());
                message.setItemName(itemService.getById(4).getName());

                // 5.交由消息队列处理
                rabbitTemplate.convertAndSend("ticket.dir.exchange", "ticket.order", JSONUtil.toJsonStr(message));

                // 6.更新redis 标记用户为已经购买
                bucket.set(true, 2, TimeUnit.MINUTES);

                return Result.success();
            }catch (Exception e){
                socketCount.incrementAndGet();
                return Result.fail("系统繁忙，稍后重试");
            }


//            Long count = stringRedisTemplate.opsForValue().decrement(TICKET_SOCKET);
//            // 2.1 更新数据库，扣减库存：方法一
//            itemService.update().eq("id", 4).setSql("socket = socket - 1");
//            // 方法二
//            UpdateWrapper<Item> wrapper = new UpdateWrapper<>();
//            wrapper.eq("id", 4).setSql("socket = socket - 1");
//            itemService.update(wrapper);

            // 3. 说明这个存库不够了，返回-1
//            if (count == null){
//                log.info("TicketKEY不存在");
//                return Result.fail("TicketKEY不存在");
//            }
//            if(count < 0){
//                stringRedisTemplate.opsForValue().increment(TICKET_SOCKET);
//                log.info("库存不足，已经将库存加回");
//                return Result.fail("库存不足，已经将库存加回");
//            }

            // 4.异步下单
//            try {
//                Message message = new Message();
//                message.setUsername(username);
//                message.setCreateTime(LocalDateTime.now());
//                message.setItemName(itemService.getById(4).getName());
//
//                // 交由消息队列处理
//                rabbitTemplate.convertAndSend("ticket.dir.exchange", "ticket.order", JSONUtil.toJsonStr(message));
//
//                stringRedisTemplate.opsForValue().set(userKey, "1", 2, TimeUnit.MINUTES);
//
//                log.info("消息异步发送成功！");
//                return Result.success();
//
//            } catch (AmqpException e) {
//                stringRedisTemplate.opsForValue().increment(TICKET_SOCKET);
//                log.error("消息发送失败");
//                throw new RuntimeException(e);
//            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            lock.unlock();
        }
    }
}
