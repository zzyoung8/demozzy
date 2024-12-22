package com.young.sky.Service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.young.sky.Entity.Item;
import org.springframework.stereotype.Service;

import java.util.List;

public interface ItemService extends IService<Item> {
    List<Item> getAllItem();
}
